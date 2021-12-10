use std::env;
use std::fs;
use std::io::{self, Error, ErrorKind};



fn main() -> io::Result<()> {
    let filename = &env::args().collect::<Vec<String>>()[1];
    let eoferr = Error::from(ErrorKind::UnexpectedEof);
    let chars: Vec<char> = fs::read_to_string(filename)?.chars().collect();
    let width = chars.iter().position(|&x| x == '\n').ok_or(eoferr)?;
    let height = chars.iter().filter(|&&x| x == '\n').count();
    let data: Vec<u32> = chars.iter()
        .map(|x| x.to_digit(10).unwrap_or(10))
        .filter(|x| x < &10).collect();
    let mut risk = 0;
    for y in 0..height {
        for x in 0..width {
            let pos = y*width+x;
            let lowpoint =
                if x > 0 { data[pos-1] > data[pos] } else { true } &&
                if x < width-1 { data[pos+1] > data[pos] } else { true } &&
                if y > 0 { data[pos-width] > data[pos] } else { true } &&
                if y < height-1 { data[pos+width] > data[pos] } else { true };                
            if lowpoint {
                risk += data[pos]+1;
            }
        }
    }
    println!("{:?}", &risk);
    
    Ok(())
}
