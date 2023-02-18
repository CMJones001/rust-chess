//! Show the  moves available to a piece on an ascii board.

use rust_chess::{valid_moves, Board, Coord, Piece, PieceType, Player};

use rust_chess::plot_moves;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut board = Board::default();
    // let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

    let new_piece_coord = Coord::from_string("e4").map_err(|e| e.to_string())?;
    let new_piece = Piece::new(PieceType::Queen, Player::White);
    board.set(new_piece_coord, Some(new_piece));

    let moves = valid_moves(&board, new_piece_coord).map_err(|e| e.to_string())?;
    let move_grid = plot_moves(&board, &moves, false);
    println!("{}", move_grid);

    Ok(())
}
