use crate::common::{Coord, PieceType, Player};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MoveType {
    Major(MajorMove),
    PawnMove(PawnMove),
    PawnCapture(PawnCapture),
    Castling(Castling),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct GameMove {
    pub move_type: MoveType,
    pub check: bool,
    pub checkmate: bool,
    pub player: Player,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Castling {
    KingSide,
    QueenSide,
}

impl From<Castling> for MoveType {
    fn from(castling: Castling) -> Self {
        MoveType::Castling(castling)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct MajorMove {
    pub piece_type: PieceType,
    pub end: Coord,
    pub capture: bool,
}

impl From<MajorMove> for MoveType {
    fn from(m: MajorMove) -> Self {
        MoveType::Major(m)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct PawnCapture {
    pub start: u8,
    pub end: Coord,
    pub en_passant: bool,
    pub promotion: Option<PieceType>,
}

impl From<PawnCapture> for MoveType {
    fn from(m: PawnCapture) -> Self {
        MoveType::PawnCapture(m)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct PawnMove {
    pub piece_type: PieceType,
    pub end: Coord,
    pub promotion: Option<PieceType>,
}

impl From<PawnMove> for MoveType {
    fn from(m: PawnMove) -> Self {
        MoveType::PawnMove(m)
    }
}
