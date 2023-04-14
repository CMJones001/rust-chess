//! Generate the list of valid moves for a given piece.
//!
//! The main function is `list_valid_moves` which takes a board and a coordinate and returns a list of
//! potential moves. `plot_moves` will take a board and a list of potential moves and return a
//! string representation of the board with the moves marked.
use crate::common::{Board, Coord, Piece, PieceType, Player};
use std::fmt::{Display, Formatter};

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
    pub piece: Piece,
    pub captures: Option<Piece>,
    pub en_passant: Option<Coord>,
}

impl Display for PotentialMove {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
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

impl PotentialMove {
    /// By default, we assume a move does not perform an en passant capture to simplify the code
    pub fn new(start: Coord, end: Coord, piece: Piece, captures: Option<Piece>) -> Self {
        PotentialMove {
            start,
            end,
            piece,
            captures,
            en_passant: None,
        }
    }

    /// Create a new potential move that performs an en passant capture
    fn new_en_passant(
        start: Coord,
        end: Coord,
        piece: Piece,
        captures: Option<Piece>,
        capture_coords: Coord,
    ) -> Self {
        PotentialMove {
            start,
            end,
            piece,
            captures,
            en_passant: Some(capture_coords),
        }
    }
}

/// Return a list of valid moves for the piece at the given location
/// TODO: Add support for castling
///
/// # Arguments
/// * `board` - The board to check
/// * `coord` - The coordinate of the piece to check
/// * `remove_checks` - If true, remove moves that would put the player in check
///              (this is important, as a piece can still threaten a king even if the actual move
///              would put the player in check)
pub fn list_valid_moves(
    board: &Board,
    coord: Coord,
    remove_checks: bool,
) -> Result<Vec<PotentialMove>, MoveError> {
    let active_piece = board.get(coord).ok_or(MoveError::EmptyPiece(coord))?;

    let piece_type = active_piece.piece_type;
    let player = active_piece.player;

    let mut moves = match piece_type {
        PieceType::Pawn => move_pawn(board, &coord, player),
        PieceType::Rook => move_rook(board, &coord, player),
        PieceType::Knight => move_knight(board, &coord, player),
        PieceType::Bishop => move_bishop(board, &coord, player),
        PieceType::Queen => move_queen(board, &coord, player),
        PieceType::King => move_king(board, &coord, player),
    };

    // For the second pass, we need to know if the player is in check
    // Perform the move and see if the player is in check
    // TODO: Might we be able to do this without cloning the board?
    // TODO: Is there a subset of moves we can check that would be faster?
    if remove_checks {
        moves.retain(|m| {
            let mut board = board.clone();
            board.push_move(*m).unwrap();
            !is_in_check(&board, player)
        });
    }
    Ok(moves)
}

pub fn is_in_check(board: &Board, player: Player) -> bool {
    let opponent_player = player.opponent();
    let opponent_pieces: Vec<_> = board
        .positions
        .iter()
        .enumerate()
        .filter_map(|(num, p)| {
            let Some(piece) = p else {
                return None;
            };
            if piece.player == opponent_player {
                let coord = Coord::from_index(num);
                Some((coord, piece.piece_type))
            } else {
                None
            }
        })
        .collect();

    opponent_pieces.into_iter().any(|(coord, _piece)| {
        let moves = list_valid_moves(board, coord, false).unwrap();
        moves.iter().any(|m| {
            if let Some(c) = m.captures {
                c.piece_type == PieceType::King
            } else {
                false
            }
        })
    })
}

/// Return a string representation of the board, with the given moves highlighted
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
                    Piece::new(PieceType::King, player),
                    Some(piece),
                ));
            }
        } else {
            // Move to empty square
            moves.push(PotentialMove::new(
                *coord,
                new_coord,
                Piece::new(PieceType::King, player),
                None,
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
                    Piece::new(PieceType::Knight, player),
                    Some(piece),
                ));
            }
        } else {
            moves.push(PotentialMove::new(
                *coord,
                new_coord,
                Piece::new(PieceType::Knight, player),
                None,
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
                        Piece::new(piece_type, player),
                        Some(piece),
                    ));
                }
                break;
            } else {
                // We can move to the empty square
                moves.push(PotentialMove::new(
                    *coord,
                    new_coord,
                    Piece::new(piece_type, player),
                    None,
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
            let move_ = PotentialMove::new(
                *coord,
                foward_coord,
                Piece::new(PieceType::Pawn, player),
                None,
            );
            moves.push(move_);

            // Double move forward if on starting rank
            let starting_rank = match player {
                Player::White => 1,
                Player::Black => 6,
            };
            if coord.rank == starting_rank {
                if let Some(foward_coord) = coord.relative(0, 2 * direction) {
                    if board.get(foward_coord).is_none() {
                        let move_ = PotentialMove::new(
                            *coord,
                            foward_coord,
                            Piece::new(PieceType::Pawn, player),
                            None,
                        );
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
                        Piece::new(PieceType::Pawn, player),
                        Some(capture_piece),
                    );
                    moves.push(move_);
                }
            }
        }
    }

    // En passant
    if let Some(last_move) = board.last_move() {
        if last_move.is_double_pawn_move() {
            let (x_last, y_last) = (last_move.end.file, last_move.end.rank);
            let (x_piece, y_piece) = (coord.file, coord.rank);
            if (x_piece as i32 - x_last as i32).abs() == 1 && y_piece == y_last {
                let new_rank = match player {
                    Player::White => y_piece + 1,
                    Player::Black => y_piece - 1,
                };

                let move_ = PotentialMove::new_en_passant(
                    *coord,
                    Coord::new(x_last, new_rank),
                    Piece::new(PieceType::Pawn, player),
                    Some(last_move.piece),
                    last_move.end,
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
    use std::str::FromStr;
    use test_case::test_case;

    #[test]
    fn test_moves_pawn() {
        let board = Board::default();
        let coord = Coord::from_str("e2").unwrap();
        let player = Player::White;

        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 2, "Pawn should have 2 moves");

        let coord = Coord::from_str("e4").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 move");
    }

    #[test]
    fn test_moves_pawn_capture() {
        let mut board = Board::default();
        let coord = Coord::from_str("e4").unwrap();
        let player = Player::White;

        let pawn = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn));

        let coord = Coord::from_str("d5").unwrap();
        let pawn = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        board.set(coord, Some(pawn));

        let coord = Coord::from_str("e4").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);

        assert_eq!(pawn_moves[0].captures, None, "First move is not a capture");
        assert_eq!(
            pawn_moves[1].captures,
            Some(Piece::new(PieceType::Pawn, Player::Black)),
            "Pawn should capture"
        );
    }

    #[test]
    fn test_moves_pawn_black() {
        let board = Board::default();
        let coord = Coord::from_str("e7").unwrap();
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

        let coord = Coord::from_str("e5").unwrap();
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
        let coord = Coord::from_str("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on h7 to h5
        let to_coord = Coord::from_str(to_pos).unwrap();
        let from_coord = Coord::from_str(from_pos).unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        let played_move = PlayedMove {
            piece: pawn_b,
            start: from_coord,
            end: to_coord,
            captures: None,
        };
        board.move_history.push(played_move);
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should not be able to capture the black pawn on d5
        let coord = Coord::from_str("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 moves");
        assert_eq!(pawn_moves[0].captures, None, "Pawn should not capture");
    }

    #[test]
    fn test_en_passant_fails_history() {
        // Test that en passant fails if the last move was not a double pawn move

        let mut board = Board::default();
        // Start the board with a white pawn on e5
        let coord = Coord::from_str("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Put the black pawn on d5, without updating the move history!
        let to_coord = Coord::from_str("d5").unwrap();
        let from_coord = Coord::from_str("d7").unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord = Coord::from_str("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 1, "Pawn should have 1 move");
        assert_eq!(pawn_moves[0].captures, None, "Pawn should not capture");
    }

    #[test_case("d7", "d5", "d6")]
    #[test_case("f7", "f5", "f6")]
    fn test_en_passant(from_pos: &str, to_pos: &str, capture_pos: &str) {
        let mut board = Board::default();

        // Start the board with a white pawn on e5
        let coord = Coord::from_str("e5").unwrap();
        let player = Player::White;
        let pawn_w = Piece {
            piece_type: PieceType::Pawn,
            player,
        };
        board.set(coord, Some(pawn_w));

        // Move the black pawn on d7 to d5
        let to_coord = Coord::from_str(to_pos).unwrap();
        let from_coord = Coord::from_str(from_pos).unwrap();
        let pawn_b = Piece {
            piece_type: PieceType::Pawn,
            player: Player::Black,
        };
        let played_move = PlayedMove {
            piece: pawn_b,
            start: from_coord,
            end: to_coord,
            captures: None,
        };
        board.move_history.push(played_move);
        board.set(to_coord, Some(pawn_b));
        board.set(from_coord, None);

        // En passant
        // White pawn on e5 should be able to capture the black pawn on d5
        let coord = Coord::from_str("e5").unwrap();
        let pawn_moves = move_pawn(&board, &coord, player);
        assert_eq!(pawn_moves.len(), 2, "Pawn should have 2 moves");
        assert_eq!(
            pawn_moves[1].captures,
            Some(Piece::new(PieceType::Pawn, Player::Black)),
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
        let coord = Coord::from_str("e4").unwrap();
        let player = Player::White;

        let bishop_moves = move_bishop(&board, &coord, player);
        assert_eq!(bishop_moves.len(), 13, "Bishop should have 13 moves");
    }

    #[test]
    fn test_bishop_with_pieces() {
        let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

        let coord = Coord::from_str("g6").unwrap();
        let black_rook = Piece {
            piece_type: PieceType::Rook,
            player: Player::Black,
        };
        board.set(coord, Some(black_rook));

        let coord = Coord::from_str("c2").unwrap();
        let white_rook = Piece {
            piece_type: PieceType::Rook,
            player: Player::White,
        };

        board.set(coord, Some(white_rook));

        let coord = Coord::from_str("e4").unwrap();
        let player = Player::White;

        let bishop_moves = move_bishop(&board, &coord, player);
        assert_eq!(bishop_moves.len(), 10, "Bishop should have 10 moves");

        let n_captures = bishop_moves.iter().filter(|m| m.captures.is_some()).count();
        assert_eq!(n_captures, 1, "Bishop should have 1 captures");
    }

    #[test]
    fn test_check() {
        let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

        let coord = Coord::from_str("e1").unwrap();
        let piece_w = Piece {
            piece_type: PieceType::King,
            player: Player::White,
        };
        board.set(coord, Some(piece_w));

        let coord = Coord::from_str("e8").unwrap();
        let piece_b = Piece {
            piece_type: PieceType::Rook,
            player: Player::Black,
        };
        board.set(coord, Some(piece_b));

        assert!(
            is_in_check(&board, Player::White),
            "White should be in check"
        );
    }

    #[test]
    fn test_pinning() {
        // Place a Q between the r and K.
        // The Q should be pinned to the K.
        // The Q can move in the rank, but not elsewhere
        let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

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

        let coord = Coord::from_str("e4").unwrap();
        let piece_q = Piece {
            piece_type: PieceType::Queen,
            player: Player::White,
        };
        board.set(coord, Some(piece_q));

        assert!(
            !is_in_check(&board, Player::White),
            "White should not be in check"
        );

        let moves = list_valid_moves(&board, coord, true).unwrap();
        assert_eq!(moves.len(), 6, "Queen should have 6 moves");

        let captures = moves.iter().filter(|m| m.captures.is_some()).count();
        assert_eq!(captures, 1, "Queen should have 1 capture");
    }

    #[test]
    fn test_force_move_out_of_check() {
        // Place a white queen threatening the black king, none of blacks
        // pieces should be able to move unless it stops check
        let mut board = Board::from_fen_position("8/8/8/8/8/8/8/8").unwrap();

        let coord = Coord::from_str("e1").unwrap();
        let piece_k = Piece {
            piece_type: PieceType::King,
            player: Player::Black,
        };
        board.set(coord, Some(piece_k));

        let coord = Coord::from_str("e8").unwrap();
        let piece_q = Piece {
            piece_type: PieceType::Queen,
            player: Player::White,
        };
        board.set(coord, Some(piece_q));

        assert!(
            is_in_check(&board, Player::Black),
            "Black should be in check"
        );

        let coord_r = Coord::from_str("a3").unwrap();
        let piece_r = Piece {
            piece_type: PieceType::Rook,
            player: Player::Black,
        };
        board.set(coord_r, Some(piece_r));

        let moves = list_valid_moves(&board, coord_r, true).unwrap();
        assert_eq!(moves.len(), 1, "Rook should only have 1 move");

        let move_ = moves[0];
        assert_eq!(move_.end.to_string(), "e3", "Rook should move to a3");
    }
}
