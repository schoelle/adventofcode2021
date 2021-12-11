#!/usr/local/bin/pike

/*
 * Advent of Code 2021
 * Day 6 puzzle
 */

int main(int argc, array(string) argv) {
  string datastr = Stdio.read_file(argv[1]);
  array(int) init = map(datastr / ",", lambda(string x) { return (int)x;});
  array(int) data = ({ 0 }) * 9;
  foreach(init, int c) {
    data[c] += 1;
  }
  for(int i=0; i < 256; i++) {
    int zeros = data[0];
    for (int j=0; j < 8; j++) {
      data[j] = data[j+1];
    }
    data[6] += zeros;
    data[8] = zeros;
    if (i == 79 || i == 255) {
      write("Iteration %d: %d\n", i+1, Array.sum(data));
    }
  }
}
