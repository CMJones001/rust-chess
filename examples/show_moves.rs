//! Show the  moves available to a piece on an ascii board.
//!
//! This is a simple example of how to use the `list_valid_moves` function
//! and how to plot the moves on an ascii board.

use rust_chess::{list_valid_moves, Board, Coord, Piece, PieceType, Player, PotentialMove};
use std::str::FromStr;

use rust_chess::plot_moves;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let pinning = pinning_example()?;
    println!("Example of pinning:");
    for pin in pinning {
        println!("{}", pin);
    }
    println!();

    println!("Example of en Passant:");
    let en_passant = en_passant_example()?;
    for line in en_passant {
        println!("{}", line);
    }

    Ok(())
}

/// Example of en passant
///
/// Here we show the moves available to the white pawn on e5
/// after the black pawn on d7 has moved to d5.
///
/// This is a unique capture where the taken piece is not on the end
/// square.
fn en_passant_example() -> Result<Vec<String>, Box<dyn std::error::Error>> {
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

    let pawn_moves = list_valid_moves(&board, coord, false).unwrap();
    let move_grid = plot_moves(&board, &pawn_moves, false);
    board_strings.push(move_grid);

    let en_passant_move = pawn_moves.iter().find(|m| m.captures.is_some()).unwrap();
    board.push_move(*en_passant_move).unwrap();

    let move_grid = board.as_ascii();
    board_strings.push(move_grid);

    Ok(board_strings)
}

/// Example of pinning
///
/// Here we show the moves available to the white queen on e4
/// when pinned by the black rook on e8. It can only move
/// on the e file.
fn pinning_example() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();
    let mut move_list = vec![];

    let coord = Coord::from_str("e1").unwrap();
    let piece_k = Piece {
        piece_type: PieceType::King,
        player: Player::White,
    };
    board.set(coord, Some(piece_k));

    let coord = Coord::from_str("e8").unwrap();
    let piece_r = Piece {
        piece_type: PieceType::Rook,
        player: Player::Black,
    };
    board.set(coord, Some(piece_r));

    let queen_coord = Coord::from_str("e4").unwrap();
    let piece_q = Piece {
        piece_type: PieceType::Queen,
        player: Player::White,
    };
    board.set(queen_coord, Some(piece_q));

    let moves = list_valid_moves(&board, queen_coord, true).map_err(|e| e.to_string())?;
    let queen_moves = plot_moves(&board, &moves, false);
    move_list.push(queen_moves);
    Ok(move_list)
}
