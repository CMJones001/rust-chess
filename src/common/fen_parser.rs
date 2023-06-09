//! FEN Parser
//!
//! This module is used to parse a FEN string into vector of optional `Piece`s, where it can
//! then be converted into a board.
use super::piece::Piece;
use super::piece::{PieceType, Player};
use owo_colors::OwoColorize;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum FENError {
    #[error("Invalid Piece type {0} at {1},{2}")]
    InvalidPieceType(char, usize, usize, usize),
    #[error("Line too short {0} at {1} != 8")]
    InvalidLineLength(usize, usize, usize),
    #[error("Expected '/' at {0},{1}")]
    ExpectedSlash(usize, usize),
    #[error("Incomplete FEN String, got to file {} on rank {}", .0, 8-.1)]
    IncompleteFEN(usize, usize),
    #[error("Extra rank at end of FEN string")]
    ExtraRank,
}

impl FENError {
    /// Pretty print the FEN error
    ///
    /// This will print the FEN string with the error highlighted in red
    pub fn pretty_print(&self, input: &str) {
        match self {
            FENError::InvalidPieceType(p, _, _, index) => {
                mark_error_at_index(input, *index);
                eprintln!("\nInvalid piece type {} at {}", p, index)
            }
            FENError::InvalidLineLength(_, _, index) => {
                mark_error_at_index(input, *index);
                eprintln!("\nInvalid line length at {}", index)
            }
            FENError::ExpectedSlash(_, index) => {
                mark_error_at_index(input, *index);
                eprintln!("\nExpected '/' at {}", index)
            }
            _ => eprintln!("Error: {self}"),
        }
    }
}

fn mark_error_at_index(input: &str, index: usize) {
    input.chars().enumerate().for_each(|(i, c)| {
        if i == index {
            eprint!("{}", c.red().bold());
        } else {
            eprint!("{c}");
        }
    });
}

pub fn parse_fen_lines(input: &str) -> Result<[Option<Piece>; 64], FENError> {
    let mut pieces = [None; 64];
    let mut file = 0;
    let mut rank = 7;

    for (index, c) in input.chars().enumerate() {
        if c.is_ascii_digit() {
            let count = c.to_digit(10).unwrap();
            for _ in 0..count {
                file += 1;
            }
        } else if c == '/' {
            if file != 8 {
                return Err(FENError::InvalidLineLength(file, rank, index));
            }
            if rank == 0 {
                return Err(FENError::ExtraRank);
            }
            file = 0;
            rank -= 1;
        } else if file == 8 && c != '/' {
            return Err(FENError::ExpectedSlash(file, index));
        } else {
            let piece = parse_piece(c).ok_or(FENError::InvalidPieceType(c, file, rank, index))?;
            pieces[file + rank * 8] = Some(piece);
            file += 1;
        }
    }

    // File is one past the end of the last piece, so it should be 8.
    if file != 8 || rank != 0 {
        return Err(FENError::IncompleteFEN(file, rank));
    }

    Ok(pieces)
}

fn parse_piece(input: char) -> Option<Piece> {
    match input {
        'P' => Some(Piece::new(PieceType::Pawn, Player::White)),
        'N' => Some(Piece::new(PieceType::Knight, Player::White)),
        'B' => Some(Piece::new(PieceType::Bishop, Player::White)),
        'R' => Some(Piece::new(PieceType::Rook, Player::White)),
        'Q' => Some(Piece::new(PieceType::Queen, Player::White)),
        'K' => Some(Piece::new(PieceType::King, Player::White)),
        'p' => Some(Piece::new(PieceType::Pawn, Player::Black)),
        'n' => Some(Piece::new(PieceType::Knight, Player::Black)),
        'b' => Some(Piece::new(PieceType::Bishop, Player::Black)),
        'r' => Some(Piece::new(PieceType::Rook, Player::Black)),
        'q' => Some(Piece::new(PieceType::Queen, Player::Black)),
        'k' => Some(Piece::new(PieceType::King, Player::Black)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Coord;
    use crate::Board;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_valid_string() {
        let input = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R";
        let result = parse_fen_lines(input);
        let result = match result {
            Ok(r) => r,
            Err(e) => panic!("Error: {:?}", e),
        };

        assert_eq!(result.len(), 64);

        let board = Board {
            positions: result,
            move_history: vec![],
        };
        let coord = Coord::from_string("a1").unwrap();
        let expected = Some(Piece::new(PieceType::Rook, Player::White));
        assert_eq!(board.get(coord), expected, "White rook is a1");

        let board_ascii = board.as_ascii();
        let expected = indoc! {"
            rnbqkbnr
            pp.ppppp
            ........
            ..p.....
            ....P...
            .....N..
            PPPP.PPP
            RNBQKB.R
            "};

        assert_eq!(board_ascii, expected);
    }

    #[test]
    fn test_invalid_piece() {
        // Invalid piece type at e7
        let input = "rnbqkbnr/pp1pXppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R";
        let result = parse_fen_lines(input);
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(matches!(error, FENError::InvalidPieceType('X', 4, 6, _)));
        error.pretty_print(input);
    }

    #[test]
    fn test_bad_line_length() {
        // Extra peice on line 7
        let input = "rnbqkbnr/pp1pppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R";
        let result = parse_fen_lines(input);
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(matches!(error, FENError::ExpectedSlash(8, 17)));
        error.pretty_print(input);
    }

    #[test]
    fn test_short_line() {
        // Remove a piece from line 5
        let input = "rnbqkbnr/pp1ppppp/7/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R";
        let result = parse_fen_lines(input);
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(matches!(error, FENError::InvalidLineLength(7, 5, _)));
        error.pretty_print(input);
    }

    #[test]
    fn test_formatted_message() {
        let input = "rnbqkbnrpp1pXppp/8/2p5/4P3/5N2/PPPP2PP/RNBQKB1R";
        let result = parse_fen_lines(input);
        assert!(result.is_err());

        let error = result.unwrap_err();
        error.pretty_print(input);
    }

    #[test]
    fn test_too_long_fen() {
        let input = "8/8/8/8/8/8/8/pppppppp/8";
        let result = parse_fen_lines(input);
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert!(matches!(error, FENError::ExtraRank));
        error.pretty_print(input);
    }
}
