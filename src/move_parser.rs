use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, newline, space0, space1},
    combinator::{all_consuming, map, opt, value},
    multi::separated_list1,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult,
};
use std::default;

use nom_supreme::ParserExt;

use crate::common::{Coord, Owner, PieceType};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum MoveType {
    Major(MajorMove),
    PawnMove(PawnMove),
    PawnCapture(PawnCapture),
    Castling(Castling),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct GameMove {
    move_type: MoveType,
    check: bool,
    checkmate: bool,
    player: Owner,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Castling {
    KingSide,
    QueenSide,
}

impl From<Castling> for MoveType {
    fn from(Castling: Castling) -> Self {
        MoveType::Castling(Castling)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct MajorMove {
    piece_type: PieceType,
    end: Coord,
    capture: bool,
}

impl From<MajorMove> for MoveType {
    fn from(m: MajorMove) -> Self {
        MoveType::Major(m)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct PawnCapture {
    start: u8,
    end: Coord,
    en_passant: bool,
    promotion: Option<PieceType>,
}

impl From<PawnCapture> for MoveType {
    fn from(m: PawnCapture) -> Self {
        MoveType::PawnCapture(m)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct PawnMove {
    piece_type: PieceType,
    end: Coord,
    promotion: Option<PieceType>,
}

impl From<PawnMove> for MoveType {
    fn from(m: PawnMove) -> Self {
        MoveType::PawnMove(m)
    }
}

type MovePair = (u64, GameMove, Option<GameMove>);

fn parse_move_list(input: &str) -> IResult<&str, Vec<MovePair>> {
    separated_list1(multispace1, parse_move_pair)(input)
}

fn parse_move_pair(input: &str) -> IResult<&str, MovePair> {
    let turn_parser = terminated(nom::character::complete::u64, pair(tag("."), space1));
    let mut move_one = map(move_parser, |(move_type, check, checkmate)| GameMove {
        move_type: move_type,
        check: check,
        checkmate: checkmate,
        player: Owner::White,
    });
    let move_two = map(move_parser, |(move_type, check, checkmate)| GameMove {
        move_type: move_type,
        check: check,
        checkmate: checkmate,
        player: Owner::Black,
    });
    let mut parser = tuple((turn_parser, terminated(move_one, space0), opt(move_two)));
    parser(input)
}

fn move_parser(input: &str) -> IResult<&str, (MoveType, bool, bool)> {
    let mut parser = map(
        tuple((
            alt((
                parse_castling,
                parse_major_capture,
                parse_major_move,
                parse_pawn_capture,
                parse_pawn_move,
            )),
            opt(tag("+")),
            opt(tag("#")),
        )),
        |(move_type, check, checkmate)| (move_type, check.is_some(), checkmate.is_some()),
    );
    parser(input)
}

fn parse_major_capture(input: &str) -> IResult<&str, MoveType> {
    let (input, (piece_type, _, end)) = tuple((major_parser, tag("x"), coord_parser))(input)?;
    let major_move = MajorMove {
        piece_type,
        end,
        capture: true,
    }
    .into();
    Ok((input, major_move))
}

fn parse_major_move(input: &str) -> IResult<&str, MoveType> {
    let (input, (piece_type, end)) = tuple((major_parser, coord_parser))(input)?;
    let major_move = MajorMove {
        piece_type,
        end,
        capture: false,
    }
    .into();
    Ok((input, major_move))
}

fn parse_pawn_capture(input: &str) -> IResult<&str, MoveType> {
    let capture_parser = tuple((
        nom::character::complete::one_of("abcdefgh"),
        tag("x"),
        coord_parser,
    ));
    let promotion = opt(preceded(tag("="), major_parser));
    let en_passant = opt(value(true, tag(" e.p.")));
    let mut parser = tuple((capture_parser, promotion, en_passant));

    let (input, ((start, _, end), promotion, en_passant)) = parser(input)?;
    let en_passant = en_passant.unwrap_or(false);

    let start = start as u8 - b'a';
    let pawn_capture = PawnCapture {
        start,
        end,
        en_passant,
        promotion,
    }
    .into();
    Ok((input, pawn_capture))
}

fn parse_pawn_move(input: &str) -> IResult<&str, MoveType> {
    let promotion = opt(preceded(tag("="), major_parser));
    let mut parser = tuple((coord_parser, promotion));

    let (input, (end, promotion)) = parser(input)?;
    let pawn_move = PawnMove {
        piece_type: PieceType::Pawn,
        end,
        promotion,
    }
    .into();
    Ok((input, pawn_move))
}

fn parse_castling(i: &str) -> IResult<&str, MoveType> {
    map(
        alt((
            value(Castling::QueenSide, tag("O-O-O")),
            value(Castling::KingSide, tag("O-O")),
            value(Castling::QueenSide, tag("0-0-0")),
            value(Castling::KingSide, tag("0-0")),
        )),
        |v| v.into(),
    )(i)
}

fn parse_check(i: &str) -> IResult<&str, bool> {
    alt((value(false, tag("+")), value(true, tag("#"))))(i)
}

fn parse_checkmate(input: &str) -> IResult<&str, bool> {
    alt((value(false, tag("")), value(true, tag("#"))))(input)
}

fn major_parser(input: &str) -> IResult<&str, PieceType> {
    alt((
        value(PieceType::Rook, tag("R")),
        value(PieceType::Bishop, tag("B")),
        value(PieceType::Knight, tag("N")),
        value(PieceType::Queen, tag("Q")),
        value(PieceType::King, tag("K")),
    ))(input)
}

fn coord_parser(input: &str) -> IResult<&str, Coord> {
    let (input, (file, rank)) = tuple((
        nom::character::complete::one_of("abcdefgh"),
        nom::character::complete::one_of("12345678"),
    ))(input)?;
    let file = file as u8 - b'a';
    let rank = rank as u8 - b'1';
    Ok((input, Coord::new(file, rank)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_castle() {
        assert_eq!(parse_castling("O-O"), Ok(("", Castling::KingSide.into())));
        assert_eq!(
            parse_castling("O-O-O"),
            Ok(("", Castling::QueenSide.into()))
        );
        assert_eq!(parse_castling("0-0"), Ok(("", Castling::KingSide.into())));
        assert_eq!(
            parse_castling("0-0-0"),
            Ok(("", Castling::QueenSide.into()))
        );
    }

    #[test]
    fn test_parse_major_capture() {
        let input = "Qxe4";
        let expected = MajorMove {
            piece_type: PieceType::Queen,
            end: Coord::from_string("e4").unwrap(),
            capture: true,
        }
        .into();
        assert_eq!(parse_major_capture(input), Ok(("", expected)));

        let input = "Rxe4";
        let expected = MajorMove {
            piece_type: PieceType::Rook,
            end: Coord::from_string("e4").unwrap(),
            capture: true,
        }
        .into();
        assert_eq!(parse_major_capture(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_capture_parser() {
        let input = "exd4";
        let expected = PawnCapture {
            start: 4,
            end: Coord::from_string("d4").unwrap(),
            en_passant: false,
            promotion: None,
        }
        .into();

        assert_eq!(parse_pawn_capture(input), Ok(("", expected)));

        let input = "gxh8";
        let expected = PawnCapture {
            start: 6,
            end: Coord::from_string("h8").unwrap(),
            en_passant: false,
            promotion: None,
        }
        .into();

        assert_eq!(parse_pawn_capture(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_capture_parser_with_en_passant() {
        let input = "exd4 e.p.";
        let expected = PawnCapture {
            start: 4,
            end: Coord::from_string("d4").unwrap(),
            en_passant: true,
            promotion: None,
        }
        .into();

        assert_eq!(parse_pawn_capture(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_capture_parser_with_promotion() {
        let input = "exd8=Q";
        let expected = PawnCapture {
            start: 4,
            end: Coord::from_string("d8").unwrap(),
            en_passant: false,
            promotion: Some(PieceType::Queen),
        }
        .into();

        assert_eq!(parse_pawn_capture(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_capture_parser_with_en_passant_and_promotion() {
        let input = "exd8=Q e.p.";
        let expected = PawnCapture {
            start: 4,
            end: Coord::from_string("d8").unwrap(),
            en_passant: true,
            promotion: Some(PieceType::Queen),
        }
        .into();

        assert_eq!(parse_pawn_capture(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_move() {
        let input = "d4";
        let expected = PawnMove {
            piece_type: PieceType::Pawn,
            end: Coord::from_string("d4").unwrap(),
            promotion: None,
        }
        .into();

        assert_eq!(parse_pawn_move(input), Ok(("", expected)));
    }

    #[test]
    fn test_pawn_move_with_promotion() {
        let input = "d8=Q";
        let expected = PawnMove {
            piece_type: PieceType::Pawn,
            end: Coord::from_string("d8").unwrap(),
            promotion: Some(PieceType::Queen),
        }
        .into();

        assert_eq!(parse_pawn_move(input), Ok(("", expected)));
    }

    #[test]
    fn test_single_move() {
        let input = "1. Nf3";
        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f3").unwrap(),
            capture: false,
        }
        .into();
        let white_expected = GameMove {
            move_type,
            check: false,
            checkmate: false,
            player: Owner::White,
        };

        let (input, (move_turn, white_move, black_move)) = parse_move_pair(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(move_turn, 1);
        assert_eq!(white_move, white_expected);
        assert_eq!(black_move, None);
    }

    #[test]
    fn test_move_pair() {
        let input = "1. Nf3 Nf6";
        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f3").unwrap(),
            capture: false,
        }
        .into();
        let white_expected = GameMove {
            move_type,
            check: false,
            checkmate: false,
            player: Owner::White,
        };

        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f6").unwrap(),
            capture: false,
        }
        .into();
        let black_expected = GameMove {
            move_type,
            check: false,
            checkmate: false,
            player: Owner::Black,
        };

        let (input, (move_turn, white_move, black_move)) = parse_move_pair(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(move_turn, 1);
        assert_eq!(white_move, white_expected);
        assert_eq!(black_move, Some(black_expected));
    }

    #[test]
    fn test_move_pair_with_check() {
        let input = "1. Nf3+ Nf6";
        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f3").unwrap(),
            capture: false,
        }
        .into();
        let white_expected = GameMove {
            move_type,
            check: true,
            checkmate: false,
            player: Owner::White,
        };

        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f6").unwrap(),
            capture: false,
        }
        .into();
        let black_expected = GameMove {
            move_type,
            check: false,
            checkmate: false,
            player: Owner::Black,
        };

        let (input, (move_turn, white_move, black_move)) = parse_move_pair(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(move_turn, 1);
        assert_eq!(white_move, white_expected);
        assert_eq!(black_move, Some(black_expected));
    }

    #[test]
    fn test_move_pair_with_checkmate() {
        let input = "1. Nf3 Nf6#";
        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f3").unwrap(),
            capture: false,
        }
        .into();
        let white_expected = GameMove {
            move_type,
            check: false,
            checkmate: false,
            player: Owner::White,
        };

        let move_type = MajorMove {
            piece_type: PieceType::Knight,
            end: Coord::from_string("f6").unwrap(),
            capture: false,
        }
        .into();
        let black_expected = GameMove {
            move_type,
            check: false,
            checkmate: true,
            player: Owner::Black,
        };

        let (input, (move_turn, white_move, black_move)) = parse_move_pair(input).unwrap();
        assert_eq!(input, "");
        assert_eq!(move_turn, 1);
        assert_eq!(white_move, white_expected);
        assert_eq!(black_move, Some(black_expected));
    }

    #[test]
    fn test_move_list_flat() {
        let input = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6";
        let (remaining, moves) = parse_move_list(input).unwrap();
        assert_eq!(remaining, "");
        assert_eq!(moves.len(), 3);
    }
}
