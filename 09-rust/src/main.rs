use std::env;
use std::fs;
use std::io::{self, Error, ErrorKind};

fn first(data: &Vec<u32>, width: usize, height: usize) {
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
}

fn get_size(data: &mut Vec<u32>, width: usize, height: usize, x: usize, y: usize) -> u32 {
    let pos = y*width+x;
    if data[pos] < 9 {
        let mut size = 1;
        data[pos] = 9;
        if x > 0 {
            size += get_size(data, width, height, x-1, y);
        }
        if x < width-1 {
            size += get_size(data, width, height, x+1, y);
        }
        if y > 0 {
            size += get_size(data, width, height, x, y-1);
        }
        if y < height-1 {
            size += get_size(data, width, height, x, y+1);
        }
        return size;
    }
    return 0;
}

fn second(data: &mut Vec<u32>, width: usize, height: usize) {
    let mut sizes: Vec<u32> = vec![];
    for y in 0..height {
        for x in 0..width {
            let size = get_size(data, width, height, x, y);
            if size > 0 {
                sizes.push(size);
            }
        }
    }
    sizes.sort();
    let mut res = 1;
    for v in sizes.iter().rev().take(3) {
        res *= v;
    }
    println!("{:?}", res);
}


fn main() -> io::Result<()> {
    let filename = &env::args().collect::<Vec<String>>()[1];
    let eoferr = Error::from(ErrorKind::UnexpectedEof);
    let chars: Vec<char> = fs::read_to_string(filename)?.chars().collect();
    let width = chars.iter().position(|&x| x == '\n').ok_or(eoferr)?;
    let height = chars.iter().filter(|&&x| x == '\n').count();
    let mut data: Vec<u32> = chars.iter()
        .map(|x| x.to_digit(10).unwrap_or(10))
        .filter(|x| x < &10).collect();
    first(&data, width, height);
    second(&mut data, width, height);
    Ok(())
}
