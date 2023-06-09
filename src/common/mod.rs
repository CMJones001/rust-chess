#![allow(dead_code)]
mod coord;
mod errors;
mod fen_parser;
mod piece;
use crate::potential_moves::PotentialMove;
use std::default::Default;
use std::fmt::Display;

pub use coord::Coord;
pub use errors::{AnParseError, BoardError, MoveError};
use fen_parser::{parse_fen_lines, FENError};
pub use piece::{Piece, PieceType, Player};

// Coordinates on the board are given by a rank and a file.
// The file is a letter from a to h, and the rank is a number from 1 to 8.
// The file are the columns, and the rank are the rows.

// We represent the board as 1d array, so we need to convert the 2d coordinates
// to 1d coordinates. Index 0 to 7 cover the first rank, 8 to 15 cover the second rank, etc.
//
// The elements in the board array are Option<Peice>, so we can represent empty squares.
#[derive(Debug, Clone, PartialEq, Eq)]
/// Representation of the chess board.
///
/// The board is represented as a 1d array of 64 elements, where each element is an `Option<Piece>`.
/// The board is indexed by `Coord`, which is a tuple of (file, rank).
/// The top left corner is (0, 0), and the bottom right corner is (7, 7).
///
/// The board also keeps track of the move history.
pub struct Board {
    pub positions: [Option<Piece>; 64],
    pub move_history: Vec<PlayedMove>,
}

impl Board {
    /// Create a new board with all squares empty.
    /// If you want to create a board with a specific position, use `from_fen_position` or
    /// the `Board::default` implementation for the standard positions.
    fn new() -> Board {
        Board {
            positions: [None; 64],
            move_history: Vec::new(),
        }
    }

    /// Create a new board with the given position.
    /// The position is given as a FEN string.
    /// See [Wikipedia](https://en.wikipedia.org/wiki/Forsyth-Edwards_Notation) for details.
    pub fn from_fen_position(fen: &str) -> Result<Board, BoardError> {
        let results = parse_fen_lines(fen)?;
        Ok(Board {
            positions: results,
            move_history: Vec::new(),
        })
    }

    /// Returns the piece at the given coordinate, or `None` if the square is empty.
    pub fn get(&self, coord: Coord) -> Option<Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8]
    }

    /// Returns a mutable reference to the piece at the given coordinate, or `None` if the
    /// square is empty.
    pub fn get_mut(&mut self, coord: Coord) -> Option<&mut Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8].as_mut()
    }

    /// Sets the piece at the given coordinate to the given piece.
    pub fn set(&mut self, coord: Coord, piece: Option<Piece>) {
        self.positions[coord.file as usize + coord.rank as usize * 8] = piece;
    }

    /// Update the board to reflect the given move, and add the move to the move history.
    ///
    /// Will return an error if the move is not logically consistent e.g. if the piece to move
    /// is not the expected piece, or if the end position is not empty or does not contain the
    /// expected piece (if the move is a capture). However, it does not check if the move is legal
    /// according to chess rules.
    pub fn push_move(&mut self, mv: PotentialMove) -> Result<(), BoardError> {
        let start_piece = self.get(mv.start);
        let end_piece = self.get(mv.end);

        if start_piece != Some(mv.piece) {
            return Err(MoveError::UnexpectedPieceToMove(mv, start_piece).into());
        }

        if let Some(en_passant) = mv.en_passant {
            let captured_piece = self.get(en_passant);
            if let Some(end) = end_piece {
                return Err(MoveError::EnPassantIntoOccupiedSquare(mv, end).into());
            } else if captured_piece != mv.captures {
                return Err(MoveError::UnexpectedEndPositionMove(mv, captured_piece).into());
            } else {
                self.set(mv.start, None);
                self.set(mv.end, Some(mv.piece));
                self.set(en_passant, None);
            }
        } else {
            let end_piece_type = if let Some(promoted_piece) = mv.promotion {
                promoted_piece
            } else {
                mv.piece
            };

            if end_piece != mv.captures {
                return Err(MoveError::UnexpectedEndPositionMove(mv, end_piece).into());
            } else {
                self.set(mv.start, None);
                self.set(mv.end, Some(end_piece_type));
            }
        }

        self.move_history.push(mv.into());
        Ok(())
    }

    /// Undo the last move.
    ///
    /// This will remove the last move from the move history, and update the board to reflect
    /// the move. Returns an error if there are no moves to undo.
    pub fn pop_move(&mut self) -> Result<PlayedMove, BoardError> {
        let mv = self.move_history.pop().ok_or(BoardError::NoMoveToUndo)?;

        // Set the moved piece back to the start position
        if mv.promotion.is_some() {
            let pawn = Piece::new(PieceType::Pawn, mv.piece.player);
            self.set(mv.start, Some(pawn));
        } else {
            self.set(mv.start, Some(mv.piece));
        }

        // Set the captured piece back to the end position
        if let Some(en_passant) = mv.en_passant {
            self.set(en_passant, mv.captures);
            self.set(mv.end, None);
        } else {
            self.set(mv.end, mv.captures);
        }

        Ok(mv)
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

    /// Returns the last move played on the board, or `None` if no move has been played.
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
    /// Create a new board with the standard chess starting position.
    fn default() -> Self {
        Board::from_fen_position("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap()
    }
}

/// A move that has been played on the board.
///
/// A `PotentialMove` is one of the moves that is possible to play, and this is converted to
/// a `PlayedMove` when the move is actually played on the board.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PlayedMove {
    pub piece: Piece,
    pub start: Coord,
    pub end: Coord,
    pub captures: Option<Piece>,
    // The coordinate of the piece that was captured by en passant.
    pub en_passant: Option<Coord>,
    pub promotion: Option<Piece>,
}

impl PlayedMove {
    // By default a move is not an en passant.
    pub fn new(
        piece: Piece,
        start: Coord,
        end: Coord,
        captures: Option<Piece>,
        en_passant: Option<Coord>,
        promotion: Option<Piece>,
    ) -> PlayedMove {
        PlayedMove {
            piece,
            start,
            end,
            captures,
            en_passant,
            promotion,
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
        // TODO: Get the actual piece for promotion.
        PlayedMove {
            piece: potential_move.piece,
            start: potential_move.start,
            end: potential_move.end,
            captures: potential_move.captures,
            en_passant: potential_move.en_passant,
            promotion: potential_move.promotion,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::list_valid_moves;
    use indoc::indoc;
    use piece::{PieceType, Player};
    use pretty_assertions::assert_eq;
    use test_case::test_case;

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

    /// Detect when a pawn is moved two squares forward.
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
            None,
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
            None,
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
            None,
            None,
        );

        assert!(!move_.is_double_pawn_move());
    }

    /// Test that we can allow for an en passant move and that
    /// the positions are updated correctly.
    #[test]
    fn test_en_passant_moves() {
        use crate::{list_valid_moves, plot_moves};
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
        let from_coord: Coord = "d7".parse().unwrap();
        let to_coord: Coord = "d5".parse().unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };

        let move_ = PotentialMove::new(from_coord, to_coord, pawn_b, None);
        board.push_move(move_).unwrap();

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord: Coord = "e5".parse().unwrap();

        let pawn_moves = list_valid_moves(&board, coord, false).unwrap();
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

    #[test]
    /// Testing a move that is not a capture.
    fn test_undo_simple_move() {
        let mut board = Board::default();
        let board_original = Board::default();
        let from_coord: Coord = "e2".parse().unwrap();
        let to_coord: Coord = "e4".parse().unwrap();
        let pawn = Piece {
            piece_type: PieceType::Pawn,
            player: Player::White,
        };

        let move_ = PotentialMove::new(from_coord, to_coord, pawn, None);
        board.push_move(move_).unwrap();

        board.pop_move().unwrap();

        assert_eq!(board, board_original);
    }

    #[test]
    /// Undoing a capture move.
    fn test_undo_capture_move() {
        let mut board = Board::new();

        // Set up a board with white rook on e4 and black queen on e6
        let from_coord: Coord = "e4".parse().unwrap();
        let to_coord: Coord = "e6".parse().unwrap();
        let rook = Piece {
            piece_type: PieceType::Rook,
            player: Player::White,
        };
        let queen = Piece {
            piece_type: PieceType::Queen,
            player: Player::Black,
        };

        board.set(from_coord, Some(rook));
        board.set(to_coord, Some(queen));

        let board_original = board.clone();
        let n_pieces = board_original
            .positions
            .iter()
            .filter(|p| p.is_some())
            .count();
        assert_eq!(n_pieces, 2);

        // White rook on e4 captures black queen on e6
        let move_ = PotentialMove::new(from_coord, to_coord, rook, Some(queen));
        board.push_move(move_).unwrap();
        let n_pieces = board.positions.iter().filter(|p| p.is_some()).count();
        // Capture removes a piece
        assert_eq!(n_pieces, 1);

        // Undo the move and check that the board is the same as before
        let move_returned = board.pop_move().unwrap();
        let expected_move: PlayedMove = move_.into();
        assert_eq!(expected_move, move_returned);
        assert_eq!(board, board_original);

        // Check that the number of pieces is the same as before
        let n_pieces = board.positions.iter().filter(|p| p.is_some()).count();
        assert_eq!(n_pieces, 2);
    }

    #[test]
    fn test_undo_promotion() {
        let mut board = Board::from_fen_position("8/4P3/8/8/8/8/8/8").unwrap();
        let board_original = board.clone();

        let from_coord: Coord = "e7".parse().unwrap();
        let to_coord: Coord = "e8".parse().unwrap();

        let potential_moves = list_valid_moves(&board, from_coord, false).unwrap();
        assert_eq!(
            potential_moves.len(),
            4,
            "There should be 4 moves for a pawn on e7 (e8Q, e8R, e8B, e8N)"
        );

        let _queen = Piece {
            piece_type: PieceType::Queen,
            player: Player::White,
        };

        // Select the move that promotes to a queen
        let selected_move = potential_moves
            .iter()
            .find(|m| matches!(m.promotion, Some(_queen)))
            .unwrap();
        board.push_move(*selected_move).unwrap();

        // There should only be the queen on e8
        let n_pieces = board.positions.iter().filter(|p| p.is_some()).count();
        assert_eq!(n_pieces, 1);
        assert_eq!(board.get(to_coord).unwrap().piece_type, PieceType::Queen);

        // Undo the move
        board.pop_move().unwrap();
        assert_eq!(board, board_original);
    }

    #[test]
    fn test_undo_en_passant() {
        let mut board = Board::default();

        // Add a white pawn on e5
        let coord = Coord::from_string("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on d7 to d5
        let from_coord: Coord = "d7".parse().unwrap();
        let to_coord: Coord = "d5".parse().unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        let move_ = PotentialMove::new(from_coord, to_coord, pawn_b, None);
        board.push_move(move_).unwrap();
        let board_original = board.clone();
        println!("{}", board);

        // En passant
        // White pawn on e5 captures the pawn on d5
        let coord: Coord = "e5".parse().unwrap();
        let pawn_moves = list_valid_moves(&board, coord, false).unwrap();
        let en_passant_move = pawn_moves.iter().find(|m| m.captures.is_some()).unwrap();
        board.push_move(*en_passant_move).unwrap();
        println!("{}", board);

        // There are now two moves in the history
        assert_eq!(board.move_history.len(), 2);

        // Undo the en passant move
        let move_returned = board.pop_move().unwrap();
        let expected_move: PlayedMove = (*en_passant_move).into();
        assert_eq!(move_returned, expected_move);
        assert_eq!(board.to_string(), board_original.to_string());
    }

    #[test]
    /// If there are no moves, undoing should fail.
    fn test_undo_fails() {
        let mut board = Board::default();
        let result = board.pop_move();

        assert!(result.is_err());
        matches!(result.unwrap_err(), BoardError::NoMoveToUndo);
    }
}
