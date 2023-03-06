use crate::{common::FENError, PotentialMove};
use thiserror::Error;

use crate::common::Piece;

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
    UnexpectedPieceToMove(PotentialMove, Option<Piece>),
    #[error("Unexpected piece on end position, expected {0:?} found {1:?}")]
    UnexpectedEndPositionMove(PotentialMove, Option<Piece>),
    #[error("En Passant into occupied square: {0}")]
    EnPassantIntoOccupiedSquare(PotentialMove, Piece),
}
