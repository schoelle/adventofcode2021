#include <stdio.h>
#include <string.h>
#include <err.h>
#include <stdlib.h>

#define MAX_SIZE 1000

int map1[MAX_SIZE][MAX_SIZE];
int map2[MAX_SIZE][MAX_SIZE];

void output(char *label, int map[MAX_SIZE][MAX_SIZE]) {
  int count = 0;
  for (int x=0; x < MAX_SIZE; x++) {
    for (int y=0; y < MAX_SIZE; y++) {
      if (map[x][y] > 1) {
	count++;
      }
    }    
  }
  printf("%s %d\n", label, count);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Call %s <input>\n", argv[0]);
    return 1;
  }
  memset(map1, 0, MAX_SIZE * MAX_SIZE * sizeof(int));
  memset(map2, 0, MAX_SIZE * MAX_SIZE * sizeof(int));
  FILE *fd = fopen(argv[1], "r");
  if (fd == NULL) {
    err(EXIT_FAILURE, "%s", argv[1]);
  }

  int x1,y1,x2,y2,tmp;
  while(!feof(fd)) {
    fscanf(fd, "%d,%d -> %d,%d\n", &x1, &y1, &x2, &y2);
    if (x1 == x2) {
      if (y1 > y2) {tmp = y1;y1 = y2;y2 = tmp;}
      for (int i = y1; i <= y2; i++) {
	map1[x1][i]++;
	map2[x1][i]++;
      }
    } else if (y1 == y2) {
      if (x1 > x2) {tmp = x1;x1 = x2;x2 = tmp;}
      for (int i = x1; i <= x2; i++) {
	map1[i][y1]++;
	map2[i][y1]++;
      }
    } else {
      if (x1 > x2) {
	tmp = x1;x1 = x2;x2 = tmp;
	tmp = y1;y1 = y2;y2 = tmp;
      }
      if (y1 > y2) {
	for (int i = x1; i <= x2; i++) {
	  map2[i][y1-i+x1]++;
	}
      } else {
	for (int i = x1; i <= x2; i++) {
	  map2[i][y1+i-x1]++;
	}
      }
    }
  }
  fclose(fd);
  output("First star: ", map1);
  output("Second star: ", map2);
}
