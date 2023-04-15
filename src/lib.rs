//! Main documentation for the library
//!
mod an_parser;
mod common;
mod potential_moves;

pub use common::{Board, BoardError, Coord, Piece, PieceType, Player};
pub use potential_moves::{list_valid_moves, plot_moves, PotentialMove};
