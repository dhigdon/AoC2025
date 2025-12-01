// Advent of Code 2025 - Day 1 - Secret Entrance

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define LOCK 100

int advance( int pt, int delta )
{
   int const newpt = pt + delta;
   int const result = newpt % LOCK;

   // Needs "real" mod operator instead of remainder
   return (result < 0) ? result + LOCK : result;
}

int main( int argc, char **argv )
{
   if (argc != 2)
   {
      perror("Usage: day1 <filename>\n");
      return 1;
   }
   FILE * fp = fopen(argv[1], "r");
   if (fp == NULL)
   {
      fprintf(stderr, "Unable to open %s\n", argv[1]);
      return 2;
   }

   int point = 50;
   int p1 = 0, p2 = 0;
   int new_point, direction;
   while (!feof(fp))
   {
      char buf[80];
      fgets(buf, sizeof(buf), fp);

      if (strlen(buf) > 0)
      {
         int rot = (int)strtol(buf + 1, NULL, 10);

         // Part 1 - count when we land on zero
         new_point = point;
         direction = (buf[0] == 'L') ? -rot : rot;
         new_point = advance( new_point, direction );
         if (new_point == 0) ++p1;
         
         // Part 2 - requires us to find each time it passes zero
         new_point = point;
         direction = (buf[0] == 'L') ? -1 : 1;
         for (int i = 0; i < rot; ++i)
         {
            new_point = advance( new_point, direction );
            if (new_point == 0) ++p2;
         }

         // Finally, "fix" our position
         point = new_point;
      }
   }

   printf("Part1 = %d\n", p1);
   printf("Part2 = %d\n", p2);
}
