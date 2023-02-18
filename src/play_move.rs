use crate::common::{Board, Coord, PieceType, Player};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum MoveError {
    #[error("No piece at coordinate: {0}")]
    EmptyPiece(Coord),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PotentialMove {
    pub start: Coord,
    pub end: Coord,
    pub piece: PieceType,
    pub captures: Option<PieceType>,
    pub player: Player,
}

impl PotentialMove {
    fn new(
        start: Coord,
        end: Coord,
        piece: PieceType,
        captures: Option<PieceType>,
        player: Player,
    ) -> Self {
        PotentialMove {
            start,
            end,
            piece,
            captures,
            player,
        }
    }
}

// Return a list of valid moves for the piece at the given location
// TODO: Perform a second pass to remove moves that would put the player in check
pub fn valid_moves(board: &Board, coord: Coord) -> Result<Vec<PotentialMove>, MoveError> {
    let active_piece = board.get(coord).ok_or(MoveError::EmptyPiece(coord))?;

    let piece_type = active_piece.piece_type;
    let player = active_piece.player;

    let moves = match piece_type {
        PieceType::Pawn => move_pawn(board, &coord, player),
        PieceType::Rook => move_rook(board, &coord, player),
        PieceType::Knight => move_knight(board, &coord, player),
        PieceType::Bishop => move_bishop(board, &coord, player),
        PieceType::Queen => move_queen(board, &coord, player),
        PieceType::King => move_king(board, &coord, player),
    };
    Ok(moves)
}

pub fn plot_moves(board: &Board, moves: &[PotentialMove], unicode: bool) -> String {
    let blank = if unicode { ' ' } else { '.' };
    let mut s = String::new();

    for rank in (0..8).rev() {
        for file in 0..8 {
            let coord = Coord { file, rank };
            s.push(
                if let Some(move_end) = moves.iter().find(|m| m.end == coord) {
                    match move_end.captures {
                        Some(_) => 'x',
                        None => 'o',
                    }
                } else if let Some(piece) = board.get(coord) {
                    if unicode {
                        piece.as_unicode()
                    } else {
                        piece.as_ascii()
                    }
                } else {
                    blank
                },
            )
        }
        s.push('\n');
    }

    s
}

fn move_bishop(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let deltas = vec![(1, 1), (1, -1), (-1, 1), (-1, -1)];
    try_linear_moves(board, coord, player, PieceType::Bishop, &deltas)
}

fn move_rook(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let deltas = vec![(0, 1), (0, -1), (1, 0), (-1, 0)];
    try_linear_moves(board, coord, player, PieceType::Rook, &deltas)
}

fn move_queen(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let deltas = vec![
        (0, 1),
        (0, -1),
        (1, 0),
        (-1, 0),
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
    ];
    try_linear_moves(board, coord, player, PieceType::Queen, &deltas)
}

fn move_king(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let mut moves = Vec::new();
    let deltas = vec![
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
        (-1, 0),
        (-1, 1),
    ];

    for (dy, dx) in deltas {
        let Some(new_coord) = coord.relative(dx, dy) else {continue};
        if let Some(piece) = board.get(new_coord) {
            if piece.player != player {
                // Capture
                moves.push(PotentialMove::new(
                    *coord,
                    new_coord,
                    PieceType::King,
                    Some(piece.piece_type),
                    player,
                ));
            }
        } else {
            // Move to empty square
            moves.push(PotentialMove::new(
                *coord,
                new_coord,
                PieceType::King,
                None,
                player,
            ));
        }
    }

    moves
}

fn move_knight(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let mut moves = Vec::new();
    let deltas: Vec<(i32, i32)> = vec![
        (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1),
    ];
    for (dy, dx) in deltas {
        let Some(new_coord) = coord.relative(dx, dy) else { continue };
        if let Some(piece) = board.get(new_coord) {
            if piece.player != player {
                moves.push(PotentialMove::new(
                    *coord,
                    new_coord,
                    PieceType::Knight,
                    Some(piece.piece_type),
                    player,
                ));
            }
        } else {
            moves.push(PotentialMove::new(
                *coord,
                new_coord,
                PieceType::Knight,
                None,
                player,
            ));
        }
    }
    moves
}

fn try_linear_moves(
    board: &Board,
    coord: &Coord,
    player: Player,
    piece_type: PieceType,
    deltas: &[(i32, i32)],
) -> Vec<PotentialMove> {
    let mut moves = Vec::new();

    for (dy, dx) in deltas.iter() {
        for i in 1..8 {
            // Break at the end of the board
            let Some(new_coord) = coord.relative(i*dy, i*dx) else { break };

            if let Some(piece) = board.get(new_coord) {
                if piece.player != player {
                    // We can capture the piece
                    moves.push(PotentialMove::new(
                        *coord,
                        new_coord,
                        piece_type,
                        Some(piece.piece_type),
                        player,
                    ));
                }
                break;
            } else {
                // We can move to the empty square
                moves.push(PotentialMove::new(
                    *coord, new_coord, piece_type, None, player,
                ));
            }
        }
    }

    moves
}

fn move_pawn(board: &Board, coord: &Coord, player: Player) -> Vec<PotentialMove> {
    let mut moves = Vec::new();

    let direction = match player {
        Player::White => 1,
        Player::Black => -1,
    };

    // Single move forward for white
    if let Some(foward_coord) = coord.relative(0, direction) {
        if board.get(foward_coord).is_none() {
            let move_ = PotentialMove::new(*coord, foward_coord, PieceType::Pawn, None, player);
            moves.push(move_);

            // Double move forward if on starting rank
            let starting_rank = match player {
                Player::White => 1,
                Player::Black => 6,
            };
            if coord.rank == starting_rank {
                if let Some(foward_coord) = coord.relative(0, 2 * direction) {
                    if board.get(foward_coord).is_none() {
                        let move_ =
                            PotentialMove::new(*coord, foward_coord, PieceType::Pawn, None, player);
                        moves.push(move_);
                    }
                }
            }
        }
    }

    // Capture moves
    for (x, y) in &[(1, direction), (-1, direction)] {
        if let Some(capture_coord) = coord.relative(*x, *y) {
            if let Some(capture_piece) = board.get(capture_coord) {
                if capture_piece.player != player {
                    let move_ = PotentialMove::new(
                        *coord,
                        capture_coord,
                        PieceType::Pawn,
                        Some(capture_piece.piece_type),
                        player,
                    );
                    moves.push(move_);
                }
            }
        }
    }

    // En passant
    if let Some(last_move) = board.last_move() {
        if last_move.is_double_pawn_move() {
            let (x_last, y_last) = (last_move.to.file, last_move.to.rank);
            let (x_piece, y_piece) = (coord.file, coord.rank);
            if (x_piece as i32 - x_last as i32).abs() == 1 && y_piece == y_last {
                let new_rank = match player {
                    Player::White => y_piece + 1,
                    Player::Black => y_piece - 1,
                } as u8;
                let move_ = PotentialMove::new(
                    *coord,
                    Coord::new(x_last, new_rank),
                    PieceType::Pawn,
                    Some(last_move.piece.piece_type),
                    player,
                );
                moves.push(move_);
            }
        }
    }

    moves
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Board, Coord, Piece, PieceType, PlayedMove, Player};
    use test_case::test_case;

    #[test]
    fn test_moves_pawn() {
        let board = Board::default();
        let coord = Coord::from_string("e2").unwrap();
        let player = Player::White;

        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 2, "Pawn should have 2 moves");

        let coord = Coord::from_string("e4").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 move");
    }

    #[test]
    fn test_moves_pawn_capture() {
        let mut board = Board::default();
        let coord = Coord::from_string("e4").unwrap();
        let player = Player::White;

        let pawn = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn));

        let coord = Coord::from_string("d5").unwrap();
        let pawn = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        board.set(coord, Some(pawn));

        let coord = Coord::from_string("e4").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);

        assert_eq!(pawn_moves[0].captures, None, "First move is not a capture");
        assert_eq!(
            pawn_moves[1].captures,
            Some(PieceType::Pawn),
            "Pawn should capture"
        );
    }

    #[test]
    fn test_moves_pawn_black() {
        let board = Board::default();
        let coord = Coord::from_string("e7").unwrap();
        let player = Player::Black;

        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 2, "Pawn should have 2 moves");
        let expected_ends = vec!["e6", "e5"];
        for (i, move_) in pawn_moves.iter().enumerate() {
            assert_eq!(
                move_.end.to_string(),
                expected_ends[i],
                "Pawn should move to {}",
                expected_ends[i]
            );
        }

        let coord = Coord::from_string("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 move");
        assert_eq!(
            pawn_moves[0].end.to_string(),
            "e4",
            "Pawn should move to e4"
        );
    }

    #[test_case("a7", "a5")]
    #[test_case("b7", "b5")]
    #[test_case("c7", "c5")]
    #[test_case("e7", "e5")]
    fn test_en_passant_fails_rank(from_pos: &str, to_pos: &str) {
        // Test that en passant fails if the pawn is not on the correct rank
        let mut board = Board::default();

        // Start the board with a white pawn on e5
        let coord = Coord::from_string("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on h7 to h5
        let to_coord = Coord::from_string(to_pos).unwrap();
        let from_coord = Coord::from_string(from_pos).unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        let played_move = PlayedMove {
            piece: pawn_b,
            from: from_coord,
            to: to_coord,
            captured: None,
        };
        board.move_history.push(played_move);
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should not be able to capture the black pawn on d5
        let coord = Coord::from_string("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 moves");
        assert_eq!(pawn_moves[0].captures, None, "Pawn should not capture");
    }

    #[test]
    fn test_en_passant_fails_history() {
        // Test that en passant fails if the last move was not a double pawn move

        let mut board = Board::default();
        // Start the board with a white pawn on e5
        let coord = Coord::from_string("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Put the black pawn on d5, without updating the move history!
        let to_coord = Coord::from_string("d5").unwrap();
        let from_coord = Coord::from_string("d7").unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord = Coord::from_string("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 move");
        assert_eq!(pawn_moves[0].captures, None, "Pawn should not capture");
    }

    #[test_case("d7", "d5", "d6")]
    #[test_case("f7", "f5", "f6")]
    fn test_en_passant(from_pos: &str, to_pos: &str, capture_pos: &str) {
        let mut board = Board::default();

        // Start the board with a white pawn on e5
        let coord = Coord::from_string("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on d7 to d5
        let to_coord = Coord::from_string(to_pos).unwrap();
        let from_coord = Coord::from_string(from_pos).unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        let played_move = PlayedMove {
            piece: pawn_b,
            from: from_coord,
            to: to_coord,
            captured: None,
        };
        board.move_history.push(played_move);
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord = Coord::from_string("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 2, "Pawn should have 2 moves");
        assert_eq!(
            pawn_moves[1].captures,
            Some(PieceType::Pawn),
            "Pawn should capture"
        );
        assert_eq!(
            pawn_moves[1].end.to_string(),
            capture_pos,
            "Pawn should capture on {}",
            capture_pos
        );
    }

    #[test]
    fn test_bishop_on_empty_board() {
        let board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();
        let coord = Coord::from_string("e4").unwrap();
        let player = Player::White;

        let bishop_moves = move_bishop(&board, &coord, player);
        assert_eq!(bishop_moves.len(), 13, "Bishop should have 13 moves");
    }

    #[test]
    fn test_bishop_with_pieces() {
        let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

        let coord = Coord::from_string("g6").unwrap();
        let black_rook = Piece {
            piece_type: PieceType::Rook,
            player: Player::Black,
        };
        board.set(coord, Some(black_rook));

        let coord = Coord::from_string("c2").unwrap();
        let white_rook = Piece {
            piece_type: PieceType::Rook,
            player: Player::White,
        };

        board.set(coord, Some(white_rook));

        let coord = Coord::from_string("e4").unwrap();
        let player = Player::White;

        let bishop_moves = move_bishop(&board, &coord, player);
        assert_eq!(bishop_moves.len(), 10, "Bishop should have 10 moves");

        let n_captures = bishop_moves.iter().filter(|m| m.captures.is_some()).count();
        assert_eq!(n_captures, 1, "Bishop should have 1 captures");
    }
}
