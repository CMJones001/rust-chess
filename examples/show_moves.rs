//! Show the  moves available to a piece on an ascii board.

use rust_chess::{valid_moves, Board, Coord, Piece, PieceType, Player};

use rust_chess::plot_moves;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut board = Board::default();

    let new_piece_coord = Coord::from_string("e7").map_err(|e| e.to_string())?;
    board.set(
        new_piece_coord,
        Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        },
    );

    let moves = valid_moves(&board, new_piece_coord).map_err(|e| e.to_string())?;
    let move_grid = plot_moves(&board, &moves, false);
    println!("{}", move_grid);

    Ok(())
}
