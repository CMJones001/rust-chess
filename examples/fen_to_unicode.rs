use clap::Parser;
use rust_chess::{Board, BoardError};

#[derive(Parser)]
struct Args {
    fen_string: String,
    #[arg(short, long)]
    unicode: bool,
}

fn main() {
    let args = Args::parse();
    let fen = args.fen_string;

    let board = Board::from_fen_position(&fen);
    match board {
        Ok(board) => {
            if args.unicode {
                println!("{}", board.as_unicode());
            } else {
                println!("{}", board.as_ascii());
            }
        }
        Err(e) => {
            if let BoardError::FenParseError(f) = e {
                f.pretty_print(&fen)
            } else {
                eprintln!("{}", e)
            }
        }
    }
}
