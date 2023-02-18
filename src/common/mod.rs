#![allow(dead_code)]
mod fen_parser;
mod piece;
use crate::play_move::PotentialMove;
use std::default::Default;
use std::fmt::Display;
use thiserror::Error;

use fen_parser::{parse_fen_lines, FENError};
pub use piece::{Piece, PieceType, Player};

#[derive(Error, Debug)]
pub enum AnParseError {
    #[error("Invalid move string: {0}")]
    InvalidMoveString(String),
    #[error("Parsing not complete: remains {0} from {1},")]
    IncompleteParse(String, String),
}

#[derive(Error, Debug)]
pub enum BoardError {
    #[error("Invalid coordinate string: {0}, expected format: [a-h][1-8]")]
    MalformedCoordinateString(String),
    #[error("Invalid coordinate: {0:?}, expected char in range: [0-7][0-7]")]
    CoordinateOutOfRange((usize, usize)),
    #[error("Unexpected number of ranks: {0}: expected 8")]
    UnexpectedNumberOfRanks(usize),
    #[error("Unexpected number of files in rank {1}: {0} expected 8")]
    UnexpectedNumberOfFiles(usize, usize),
    #[error("No King found for {0}")]
    NoKingFound(Player),
    #[error(transparent)]
    FenParseError(#[from] FENError),
    #[error(transparent)]
    AnParseError(#[from] AnParseError),
    #[error(transparent)]
    MoveError(#[from] MoveError),
}

#[derive(Error, Debug)]
pub enum MoveError {
    #[error("Unexpected piece to move, expected {0:?} found {1:?}")]
    UnexpectedPieceToMove(Piece, Option<Piece>),
    #[error("Unexpected piece on end position, expected {0:?} found {1:?}")]
    UnexpectedEndPositionMove(Option<Piece>, Option<Piece>),
    #[error("En Passant into occupied square: {0}")]
    EnPassantIntoOccupiedSquare(Piece),
}

// Coordinates on the board are given by a rank and a file.
// The file is a letter from a to h, and the rank is a number from 1 to 8.
// The file are the columns, and the rank are the rows.

// We represent the board as 1d array, so we need to convert the 2d coordinates
// to 1d coordinates. Index 0 to 7 cover the first rank, 8 to 15 cover the second rank, etc.
//
// The elements in the board array are Option<Peice>, so we can represent empty squares.
#[derive(Debug, Clone)]
pub struct Board {
    pub positions: [Option<Piece>; 64],
    pub move_history: Vec<PlayedMove>,
}

impl Board {
    fn new() -> Board {
        Board {
            positions: [None; 64],
            move_history: Vec::new(),
        }
    }

    pub fn from_fen_position(fen: &str) -> Result<Board, BoardError> {
        let results = parse_fen_lines(fen)?;
        Ok(Board {
            positions: results,
            move_history: Vec::new(),
        })
    }

    pub fn get(&self, coord: Coord) -> Option<Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8]
    }

    pub fn get_mut(&mut self, coord: Coord) -> Option<&mut Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8].as_mut()
    }

    pub fn set(&mut self, coord: Coord, piece: Option<Piece>) {
        self.positions[coord.file as usize + coord.rank as usize * 8] = piece;
    }

    pub fn push_move(&mut self, mv: PotentialMove) -> Result<(), BoardError> {
        // Test that the piece moved is the piece we expect to move.

        if let Some(en_passant) = mv.en_passant {
            let start_piece = self.get(mv.start);
            let end_piece = self.get(mv.end);
            let captured_piece = self.get(en_passant);
            if start_piece != Some(mv.piece) {
                return Err(MoveError::UnexpectedPieceToMove(mv.piece, start_piece).into());
            } else if end_piece != None {
                return Err(MoveError::EnPassantIntoOccupiedSquare(end_piece.unwrap()).into());
            } else if captured_piece != mv.captures {
                return Err(
                    MoveError::UnexpectedEndPositionMove(mv.captures, captured_piece).into(),
                );
            } else {
                self.set(mv.start, None);
                self.set(mv.end, Some(mv.piece));
                self.set(en_passant, None);
            }
        } else {
            let start_piece = self.get(mv.start);
            let end_peice = self.get(mv.end);
            if start_piece != Some(mv.piece) {
                return Err(MoveError::UnexpectedPieceToMove(mv.piece, start_piece).into());
            } else if end_peice != mv.captures {
                return Err(MoveError::UnexpectedEndPositionMove(mv.captures, end_peice).into());
            } else {
                self.set(mv.start, None);
                self.set(mv.end, Some(mv.piece));
            }
        }

        self.move_history.push(mv.into());
        Ok(())
    }

    /// Give the positions of the board in ascii format.
    /// The board is printed with the white pieces, ie rank 1, on the bottom.
    /// Terminated with a newline.
    pub fn as_ascii(&self) -> String {
        self.as_grid(false, '.')
    }

    pub fn as_unicode(&self) -> String {
        self.as_grid(true, ' ')
    }

    fn as_grid(&self, unicode: bool, empty_char: char) -> String {
        let mut s = String::new();

        // Iterate over the ranks in reverse order, so that rank 1 is printed last.
        self.positions.rchunks_exact(8).for_each(|row| {
            row.iter().for_each(|piece| {
                s.push(match piece {
                    Some(piece) => {
                        if unicode {
                            piece.as_unicode()
                        } else {
                            piece.as_ascii()
                        }
                    }
                    None => empty_char,
                });
            });
            s.push('\n');
        });
        s
    }

    pub fn last_move(&self) -> Option<PlayedMove> {
        self.move_history.last().copied()
    }

    /// Convert a grid of vector positions to a board.
    ///
    /// The grid is expected to have 8 rows, and each row should have 8 elements.
    /// Following the FEN notation, the first row is the black pieces, and the last row is the white pieces.
    fn from_vec(vec: Vec<Vec<Option<Piece>>>) -> Result<Board, BoardError> {
        if vec.len() != 8 {
            return Err(BoardError::UnexpectedNumberOfRanks(vec.len()));
        }

        let mut board = Board::new();
        for (rank, rank_vec) in vec.iter().enumerate() {
            if rank_vec.len() != 8 {
                return Err(BoardError::UnexpectedNumberOfFiles(
                    rank_vec.len(),
                    7 - rank,
                ));
            }

            for (file, piece) in rank_vec.iter().enumerate() {
                // The board is stored in the opposite order, so we need to reverse the rank.
                let rank = 7 - rank;
                board.positions[file + rank * 8] = *piece;
            }
        }

        Ok(board)
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ascii())
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::from_fen_position("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Coord {
    pub file: u8,
    pub rank: u8,
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let file = self.file + 97;
        let file = file as char;
        let rank = self.rank + 1;
        write!(f, "{}{}", file, rank)
    }
}

impl Coord {
    pub fn from_string(s: &str) -> Result<Coord, BoardError> {
        if s.len() != 2 {
            return Err(BoardError::MalformedCoordinateString(s.to_string()));
        }
        let mut chars = s.chars();
        let (file, rank) = (chars.next().unwrap(), chars.next().unwrap());
        let file = file as u8;
        let rank = rank as u8;
        if !(b'a'..=b'h').contains(&file) || !(b'1'..=b'8').contains(&rank) {
            return Err(BoardError::CoordinateOutOfRange((
                file as usize,
                rank as usize,
            )));
        }
        let file = file - 97;
        let rank = rank - 49;
        Ok(Coord { file, rank })
    }

    pub fn new(file: u8, rank: u8) -> Coord {
        if file > 7 || rank > 7 {
            panic!("Invalid coordinate: {}{}", file, rank);
        }
        Coord { file, rank }
    }

    pub fn from_index(index: usize) -> Coord {
        let file = (index % 8) as u8;
        let rank = (index / 8) as u8;
        Coord { file, rank }
    }

    /// Return the coordinate relative to the given value.
    /// If the coordinate is outside the board, return None.
    ///  dy is the change in rank, and dx is the change in file.
    ///  with dy = 1, dx = 0, we get the square above the current square,
    ///  toward the black pieces.
    pub fn relative(&self, dfile: i32, drank: i32) -> Option<Coord> {
        let file = self.file as i32 + dfile;
        let rank = self.rank as i32 + drank;
        if !(0..=7).contains(&file) || !(0..=7).contains(&rank) {
            None
        } else {
            Some(Coord {
                file: file as u8,
                rank: rank as u8,
            })
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PlayedMove {
    pub piece: Piece,
    pub start: Coord,
    pub end: Coord,
    pub captures: Option<Piece>,
}

impl PlayedMove {
    pub fn new(piece: Piece, start: Coord, end: Coord, captures: Option<Piece>) -> PlayedMove {
        PlayedMove {
            piece,
            start,
            end,
            captures,
        }
    }

    pub fn is_double_pawn_move(&self) -> bool {
        match self.piece {
            Piece {
                piece_type: PieceType::Pawn,
                player: Player::White,
            } => self.start.rank == 1 && self.end.rank == 3,
            Piece {
                piece_type: PieceType::Pawn,
                player: Player::Black,
            } => self.start.rank == 6 && self.end.rank == 4,
            _ => false,
        }
    }
}

impl Display for PlayedMove {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let capture_string = match self.captures {
            Some(p) => format!("x{}", p),
            None => "".to_string(),
        };
        write!(
            f,
            "{}: {} -> {}{}",
            self.piece, self.start, self.end, capture_string
        )
    }
}

impl From<PotentialMove> for PlayedMove {
    fn from(potential_move: PotentialMove) -> Self {
        PlayedMove {
            piece: potential_move.piece,
            start: potential_move.start,
            end: potential_move.end,
            captures: potential_move.captures,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use piece::{PieceType, Player};
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test]
    fn test_coord_print() {
        let coord = Coord { file: 0, rank: 0 };
        assert_eq!(coord.to_string(), "a1");

        let coord = Coord { file: 7, rank: 7 };
        assert_eq!(coord.to_string(), "h8");

        let coord = Coord { file: 4, rank: 2 };
        assert_eq!(coord.to_string(), "e3");
    }

    #[test]
    fn test_coord_from_string() {
        let coord = Coord::from_string("a1").unwrap();
        assert_eq!(coord.file, 0);
        assert_eq!(coord.rank, 0);

        let coord = Coord::from_string("h8").unwrap();
        assert_eq!(coord.file, 7);
        assert_eq!(coord.rank, 7);

        let coord = Coord::from_string("e3").unwrap();
        assert_eq!(coord.file, 4);
        assert_eq!(coord.rank, 2);
    }

    #[test]
    fn test_coord_from_string_invalid() {
        let coord = Coord::from_string("a");
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::MalformedCoordinateString(_)
        ));

        let coord = Coord::from_string("a1a");
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::MalformedCoordinateString(_)
        ));

        let coord = Coord::from_string("a9");
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::CoordinateOutOfRange(_)
        ));

        let coord = Coord::from_string("i1");
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::CoordinateOutOfRange(_)
        ));
    }

    #[test]
    fn test_board_get() {
        let mut board = Board::new();
        let coord = Coord::new(0, 0);
        assert_eq!(board.get(coord), None);
        let coord = Coord::new(7, 7);
        assert_eq!(board.get(coord), None);
        let coord = Coord::new(4, 2);
        assert_eq!(board.get(coord), None);

        let peice = Piece::new(PieceType::Pawn, Player::White);

        board.positions[0] = Some(peice);
        let coord = Coord::new(0, 0);
        assert_eq!(board.get(coord), Some(peice));

        board.positions[7] = Some(peice);
        let coord = Coord::from_string("h1").unwrap();
        assert_eq!(board.get(coord), Some(peice));
    }

    #[test]
    fn test_board_to_ascii() {
        let mut board = Board::new();
        board.positions[0] = Some(Piece::new(PieceType::Pawn, Player::White));

        let expected = indoc!(
            "
          ........
          ........
          ........
          ........
          ........
          ........
          ........
          P.......
          "
        );
        assert_eq!(
            board.as_ascii(),
            expected,
            "White pawn in a1 at the bottom!"
        );
    }

    #[test]
    fn test_relative_coords() {
        let coord = Coord::from_string("g5").unwrap();
        assert_eq!(
            coord.relative(1, 0),
            Some(Coord::from_string("h5").unwrap())
        );
        assert_eq!(
            coord.relative(1, 1),
            Some(Coord::from_string("h6").unwrap())
        );
        assert_eq!(
            coord.relative(0, 3),
            Some(Coord::from_string("g8").unwrap())
        );

        assert_eq!(
            coord.relative(-1, 0),
            Some(Coord::from_string("f5").unwrap())
        );
        assert_eq!(
            coord.relative(-1, -1),
            Some(Coord::from_string("f4").unwrap())
        );

        assert_eq!(coord.relative(2, 0), None);
        assert_eq!(coord.relative(0, 4), None);
    }

    #[test_case("a2", "a4", Player::White; "a file, white")]
    #[test_case("d2", "d4", Player::White; "d file, white")]
    #[test_case("a7", "a5", Player::Black; "a file, black")]
    #[test_case("e7", "e5", Player::Black; "e file, black")]
    fn test_is_double_pawn_move(from: &str, to: &str, player: Player) {
        let move_ = PlayedMove::new(
            Piece::new(PieceType::Pawn, player),
            Coord::from_string(from).unwrap(),
            Coord::from_string(to).unwrap(),
            None,
        );

        assert!(move_.is_double_pawn_move());
    }

    #[test_case("a2", "a3", Player::White; "single move")]
    #[test_case("d2", "d3", Player::White; "triple move?")]
    #[test_case("a7", "a5", Player::White; "wrong player")]
    #[test_case("e7", "f6", Player::Black; "capture")]
    fn test_is_not_double_pawn_move(from: &str, to: &str, player: Player) {
        let move_ = PlayedMove::new(
            Piece::new(PieceType::Pawn, player),
            Coord::from_string(from).unwrap(),
            Coord::from_string(to).unwrap(),
            None,
        );

        assert!(!move_.is_double_pawn_move());
    }

    #[test]
    fn test_is_not_double_pawn_move_wrong_piece() {
        let move_ = PlayedMove::new(
            Piece::new(PieceType::Rook, Player::White),
            Coord::from_string("a2").unwrap(),
            Coord::from_string("a4").unwrap(),
            None,
        );

        assert!(!move_.is_double_pawn_move());
    }

    #[test]
    /// Test that we can allow for an en passant move and that
    /// the positions are updated correctly.
    fn test_en_passant_moves() {
        use crate::{plot_moves, valid_moves};
        let mut board_strings = vec![];
        let mut board = Board::default();

        let coord = Coord::from_string("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on d7 to d5
        let from_coord = Coord::from_string("d7").unwrap();
        let to_coord = Coord::from_string("d5").unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };

        let move_ = PotentialMove::new(from_coord, to_coord, pawn_b, None);
        board.push_move(move_).unwrap();

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord = Coord::from_string("e5").unwrap();

        let pawn_moves = valid_moves(&board, coord, false).unwrap();
        let move_grid = plot_moves(&board, &pawn_moves, false);
        board_strings.push(move_grid);

        let en_passant_move = pawn_moves.iter().find(|m| m.captures.is_some()).unwrap();
        board.push_move(*en_passant_move).unwrap();

        let move_grid = board.as_ascii();
        board_strings.push(move_grid);

        let expected_start = indoc!(
            "
            rnbqkbnr
            ppp.pppp
            ...xo...
            ...pP...
            ........
            ........
            PPPPPPPP
            RNBQKBNR
            "
        );
        let expected_end = indoc!(
            "
            rnbqkbnr
            ppp.pppp
            ...P....
            ........
            ........
            ........
            PPPPPPPP
            RNBQKBNR
            "
        );

        assert_eq!(board_strings[0], expected_start);
        assert_eq!(board_strings[1], expected_end);
    }
}
