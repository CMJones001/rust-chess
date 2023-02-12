#![allow(dead_code)]
mod parse;
mod piece;
use std::fmt::Display;
use thiserror::Error;

use piece::Piece;

#[derive(Error, Debug)]
pub enum BoardError {
    #[error("Invalid coordinate string: {0}, expected format: [a-h][1-8]")]
    MalformedCoordinateString(String),
    #[error("Invalid coordinate: {0:?}, expected range: [a-h][1-8]")]
    CoordinateOutOfRange((usize, usize)),
    #[error("Unexpected board size: {0}, expected size: 64=8x8")]
    UnexpectedBoardSize(usize),
}

// Coordinates on the board are given by a rank and a file.
// The file is a letter from a to h, and the rank is a number from 1 to 8.
// The file are the columns, and the rank are the rows.

// We represent the board as 1d array, so we need to convert the 2d coordinates
// to 1d coordinates. Index 0 to 7 cover the first rank, 8 to 15 cover the second rank, etc.
//
// The elements in the board array are Option<Peice>, so we can represent empty squares.
struct Board {
    positions: [Option<Piece>; 64],
}

impl Board {
    fn new() -> Board {
        Board {
            positions: [None; 64],
        }
    }

    fn get(&self, coord: Coord) -> Option<&Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8].as_ref()
    }

    fn get_mut(&mut self, coord: Coord) -> Option<&mut Piece> {
        self.positions[coord.file as usize + coord.rank as usize * 8].as_mut()
    }

    fn to_ascii(&self) -> String {
        let blank = '.';
        let mut s = String::new();
        self.positions.iter().enumerate().for_each(|(i, p)| {
            if i != 0 && i % 8 == 0 {
                s.push('\n');
            }
            s.push(match p {
                Some(p) => p.to_ascii(),
                None => blank,
            });
        });
        s
    }

    fn from_vec(vec: Vec<Option<Piece>>) -> Result<Board, BoardError> {
        if vec.len() != 64 {
            return Err(BoardError::UnexpectedBoardSize(vec.len()));
        }

        let positions = vec.try_into().unwrap();
        Ok(Board { positions })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Coord {
    file: u8,
    rank: u8,
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
    fn from_string(s: &str) -> Result<Coord, BoardError> {
        if s.len() != 2 {
            return Err(BoardError::MalformedCoordinateString(s.to_string()));
        }
        let mut chars = s.chars();
        let (file, rank) = (chars.next().unwrap(), chars.next().unwrap());
        let file = file as u8;
        let rank = rank as u8;
        if file < b'a' || file > b'h' || rank < b'1' || rank > b'8' {
            return Err(BoardError::CoordinateOutOfRange((
                file as usize,
                rank as usize,
            )));
        }
        let file = file - 97;
        let rank = rank - 49;
        Ok(Coord { file, rank })
    }

    fn new(file: u8, rank: u8) -> Coord {
        if file > 7 || rank > 7 {
            panic!("Invalid coordinate: {}{}", file, rank);
        }
        Coord { file, rank }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use piece::{Owner, PieceType};
    use pretty_assertions::assert_eq;

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

        let peice = Piece::new(PieceType::Pawn, Owner::White);

        board.positions[0] = Some(peice);
        let coord = Coord::new(0, 0);
        assert_eq!(board.get(coord), Some(&peice));

        board.positions[7] = Some(peice);
        let coord = Coord::from_string("h1").unwrap();
        assert_eq!(board.get(coord), Some(&peice));
    }

    #[test]
    fn test_board_to_ascii() {
        let mut board = Board::new();
        board.positions[0] = Some(Piece::new(PieceType::Pawn, Owner::White));
        board.positions[7] = Some(Piece::new(PieceType::Rook, Owner::White));
        board.positions[8] = Some(Piece::new(PieceType::Queen, Owner::White));
        board.positions[15] = Some(Piece::new(PieceType::King, Owner::White));

        board.positions[48] = Some(Piece::new(PieceType::Pawn, Owner::Black));
        board.positions[55] = Some(Piece::new(PieceType::Rook, Owner::Black));
        board.positions[56] = Some(Piece::new(PieceType::Queen, Owner::Black));
        board.positions[63] = Some(Piece::new(PieceType::King, Owner::Black));

        let expected = indoc!(
            "
          P......R
          Q......K
          ........
          ........
          ........
          ........
          p......r
          q......k"
        );
        assert_eq!(board.to_ascii(), expected);
    }
}
