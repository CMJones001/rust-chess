//! Show the  moves available to a piece on an ascii board.

use rust_chess::{valid_moves, Board, Coord, Piece, PieceType, Player, PotentialMove};

use rust_chess::plot_moves;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let pinning = pinning_example()?;
    for pin in pinning {
        println!("{}", pin);
    }
    println!();

    let en_passant = en_passant_example()?;
    for line in en_passant {
        println!("{}", line);
    }

    Ok(())
}

fn general_moves() -> Result<String, Box<dyn std::error::Error>> {
    // let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();
    let mut board = Board::default();

    let new_piece_coord = Coord::from_string("e4").map_err(|e| e.to_string())?;
    let new_piece = Piece::new(PieceType::Queen, Player::White);
    board.set(new_piece_coord, Some(new_piece));

    let moves = valid_moves(&board, new_piece_coord, true).map_err(|e| e.to_string())?;
    let move_grid = plot_moves(&board, &moves, false);
    Ok(move_grid)
}

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

    let pawn_moves = valid_moves(&board, coord, false).unwrap();
    let move_grid = plot_moves(&board, &pawn_moves, false);
    board_strings.push(move_grid);

    let en_passant_move = pawn_moves.iter().find(|m| m.captures.is_some()).unwrap();
    board.push_move(*en_passant_move).unwrap();

    let move_grid = board.as_ascii();
    board_strings.push(move_grid);

    Ok(board_strings)
}

fn pinning_example() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();
    let mut move_list = vec![];

    let coord = Coord::from_string("e1").unwrap();
    let piece_k = Piece {
        piece_type: PieceType::King,
        player: Player::White,
    };
    board.set(coord, Some(piece_k));

    let coord = Coord::from_string("e8").unwrap();
    let piece_r = Piece {
        piece_type: PieceType::Rook,
        player: Player::Black,
    };
    board.set(coord, Some(piece_r));

    let queen_coord = Coord::from_string("e4").unwrap();
    let piece_q = Piece {
        piece_type: PieceType::Queen,
        player: Player::White,
    };
    board.set(queen_coord, Some(piece_q));

    let moves = valid_moves(&board, queen_coord, true).map_err(|e| e.to_string())?;
    let queen_moves = plot_moves(&board, &moves, false);
    move_list.push(queen_moves);
    Ok(move_list)
}
