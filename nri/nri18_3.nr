NI18_3 := rec(
  group_name := "18/3",
  group_generators := [ (2,3)(4,5,6), (1,2)(4,5,6) ],
  elements := rec(
      1 := (),
      2 := (4,5,6),
      3 := (4,6,5),
      4 := (2,3),
      5 := (2,3)(4,5,6),
      6 := (2,3)(4,6,5),
      7 := (1,2),
      8 := (1,2)(4,5,6),
      9 := (1,2)(4,6,5),
      10 := (1,2,3),
      11 := (1,2,3)(4,5,6),
      12 := (1,2,3)(4,6,5),
      13 := (1,3,2),
      14 := (1,3,2)(4,5,6),
      15 := (1,3,2)(4,6,5),
      16 := (1,3),
      17 := (1,3)(4,5,6),
      18 := (1,3)(4,6,5) ),
  group_endomorphisms := rec(
      1 := [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
      2 := [ 1, 1, 1, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 4, 4, 4 ],
      3 := [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 10, 10, 10, 13, 13, 13, 16, 16, 16 ],
      4 := [ 1, 1, 1, 4, 4, 4, 16, 16, 16, 13, 13, 13, 10, 10, 10, 7, 7, 7 ],
      5 := [ 1, 1, 1, 7, 7, 7, 4, 4, 4, 13, 13, 13, 10, 10, 10, 16, 16, 16 ],
      6 := [ 1, 1, 1, 7, 7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 1, 7, 7, 7 ],
      7 := [ 1, 1, 1, 7, 7, 7, 16, 16, 16, 10, 10, 10, 13, 13, 13, 4, 4, 4 ],
      8 := [ 1, 1, 1, 16, 16, 16, 4, 4, 4, 10, 10, 10, 13, 13, 13, 7, 7, 7 ],
      9 := [ 1, 1, 1, 16, 16, 16, 7, 7, 7, 13, 13, 13, 10, 10, 10, 4, 4, 4 ],
      10 := [ 1, 1, 1, 16, 16, 16, 16, 16, 16, 1, 1, 1, 1, 1, 1, 16, 16, 16 ],
      11 := [ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3 ],
      12 := [ 1, 2, 3, 4, 5, 6, 4, 5, 6, 1, 2, 3, 1, 2, 3, 4, 5, 6 ],
      13 := [ 1, 2, 3, 4, 5, 6, 16, 17, 18, 13, 14, 15, 10, 11, 12, 7, 8, 9 ],
      14 := [ 1, 2, 3, 7, 8, 9, 4, 5, 6, 13, 14, 15, 10, 11, 12, 16, 17, 18 ],
      15 := [ 1, 2, 3, 7, 8, 9, 7, 8, 9, 1, 2, 3, 1, 2, 3, 7, 8, 9 ],
      16 := [ 1, 2, 3, 7, 8, 9, 16, 17, 18, 10, 11, 12, 13, 14, 15, 4, 5, 6 ],
      17 := [ 1, 2, 3, 16, 17, 18, 4, 5, 6, 10, 11, 12, 13, 14, 15, 7, 8, 9 ],
      18 := [ 1, 2, 3, 16, 17, 18, 7, 8, 9, 13, 14, 15, 10, 11, 12, 4, 5, 6 ],
      19 := [ 1, 2, 3, 16, 17, 18, 16, 17, 18, 1, 2, 3, 1, 2, 3, 16, 17, 18 ],
      20 := [ 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2 ],
      21 := [ 1, 3, 2, 4, 6, 5, 4, 6, 5, 1, 3, 2, 1, 3, 2, 4, 6, 5 ],
      22 := [ 1, 3, 2, 4, 6, 5, 7, 9, 8, 10, 12, 11, 13, 15, 14, 16, 18, 17 ],
      23 := [ 1, 3, 2, 4, 6, 5, 16, 18, 17, 13, 15, 14, 10, 12, 11, 7, 9, 8 ],
      24 := [ 1, 3, 2, 7, 9, 8, 4, 6, 5, 13, 15, 14, 10, 12, 11, 16, 18, 17 ],
      25 := [ 1, 3, 2, 7, 9, 8, 7, 9, 8, 1, 3, 2, 1, 3, 2, 7, 9, 8 ],
      26 := [ 1, 3, 2, 7, 9, 8, 16, 18, 17, 10, 12, 11, 13, 15, 14, 4, 6, 5 ],
      27 := [ 1, 3, 2, 16, 18, 17, 4, 6, 5, 10, 12, 11, 13, 15, 14, 7, 9, 8 ],
      28 := [ 1, 3, 2, 16, 18, 17, 7, 9, 8, 13, 15, 14, 10, 12, 11, 4, 6, 5 ],
      29 := [ 1, 3, 2, 16, 18, 17, 16, 18, 17, 1, 3, 2, 1, 3, 2, 16, 18, 17 ],
      30 := [ 1, 10, 13, 1, 10, 13, 1, 10, 13, 1, 10, 13, 1, 10, 13, 1, 10, 13 ],
      31 := [ 1, 11, 15, 1, 11, 15, 1, 11, 15, 1, 11, 15, 1, 11, 15, 1, 11, 15 ],
      32 := [ 1, 12, 14, 1, 12, 14, 1, 12, 14, 1, 12, 14, 1, 12, 14, 1, 12, 14 ],
      33 := [ 1, 13, 10, 1, 13, 10, 1, 13, 10, 1, 13, 10, 1, 13, 10, 1, 13, 10 ],
      34 := [ 1, 14, 12, 1, 14, 12, 1, 14, 12, 1, 14, 12, 1, 14, 12, 1, 14, 12 ],
      35 := [ 1, 15, 11, 1, 15, 11, 1, 15, 11, 1, 15, 11, 1, 15, 11, 1, 15, 11 ],
      36 := [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 ] ),
  classes := rec(
      1 := rec(
          phi := [ 1, 11, 20, 2, 12, 21, 3, 36, 22, 30, 31, 32, 33, 34, 35, 4, 13, 23 ]
           ,
          autos_yielding_iso_nrs := [ 13, 14, 16, 17, 18, 22, 23, 24, 26, 27, 28, 36 ] 
         ),
      2 := rec(
          phi := [ 1, 11, 20, 2, 12, 21, 3, 36, 22, 30, 31, 32, 33, 34, 35, 10, 19, 29 
             ],
          autos_yielding_iso_nrs := [ 13, 14, 22, 23, 24, 36 ] ),
      3 := rec(
          phi := [ 1, 11, 20, 2, 12, 21, 6, 13, 23, 30, 31, 32, 33, 34, 35, 10, 36, 22 
             ],
          autos_yielding_iso_nrs := [ 13, 14, 16, 17, 18, 22, 23, 24, 26, 27, 28, 36 ] 
         ),
      4 := rec(
          phi := [ 1, 11, 20, 2, 12, 21, 6, 15, 25, 30, 31, 32, 33, 34, 35, 10, 36, 22 
             ],
          autos_yielding_iso_nrs := [ 13, 16, 22, 23, 26, 36 ] ),
      5 := rec(
          phi := [ 1, 11, 20, 2, 12, 21, 6, 15, 25, 30, 31, 32, 33, 34, 35, 10, 36, 24 
             ],
          autos_yielding_iso_nrs := [ 13, 16, 22, 23, 26, 36 ] ),
      6 := rec(
          phi := [ 1, 11, 20, 2, 16, 26, 6, 17, 27, 30, 31, 32, 33, 34, 35, 10, 36, 22 
             ],
          autos_yielding_iso_nrs := [ 13, 16, 22, 23, 26, 36 ] ),
      7 := rec(
          phi := [ 1, 11, 20, 2, 16, 28, 6, 17, 23, 30, 31, 32, 33, 34, 35, 10, 36, 24 
             ],
          autos_yielding_iso_nrs := [ 13, 16, 22, 23, 26, 36 ] ),
      8 := rec(
          phi := [ 1, 11, 20, 3, 36, 22, 7, 16, 26, 30, 31, 32, 33, 34, 35, 8, 17, 27 ]
           ,
          autos_yielding_iso_nrs := [ 14, 17, 22, 24, 27, 36 ] ) ) );
