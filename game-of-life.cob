       identification division.
       program-id. game-of-life.
       author. socratesUK.
       data division.
       working-storage section.
       01 dimension pic 9(2) value 10.
       01 old-world.
         05 old-rows occurs 10 times.
           10 old-columns occurs 10 times.
             15 pic 9 value 0.
       01 new-world.
         05 new-rows occurs 10 times.
           10 new-columns occurs 10 times.
             15 pic 9 value 0.
       01 row-counter pic 9(2) value 1.
       01 column-counter pic 9(2) value 1.
       01 neighbours pic 9.
       procedure division.
           move 1 to old-columns(4,4).
           move 1 to old-columns(5,4).
           move 1 to old-columns(6,4).

           perform iterate-rows.
           perform print-world.
       stop run.

       print-world.
           display '---- BEFORE ----'
           perform display-old-row varying row-counter from 1 by 1 until row-counter > dimension.
           display '---- AFTER ----'
           perform display-new-row varying row-counter from 1 by 1 until row-counter > dimension.
       display-old-row.
           display old-rows(row-counter).
       display-new-row.
           display new-rows(row-counter).

       iterate-rows.
           perform iterate-columns varying row-counter from 1 by 1 until row-counter > dimension.
       iterate-columns.
           perform check-neighbours varying column-counter from 1 by 1 until column-counter > dimension.
       check-neighbours.
           move 0 to neighbours.
           if old-columns(row-counter - 1, column-counter - 1) = 1 then
             add 1 to neighbours
           end-if
           if old-columns(row-counter - 1, column-counter) = 1 then
             add 1 to neighbours
           end-if
           if old-columns(row-counter - 1, column-counter + 1) = 1 then
             add 1 to neighbours
           end-if

           if old-columns(row-counter + 1, column-counter + 1) = 1 then
             add 1 to neighbours
           end-if
           if old-columns(row-counter + 1, column-counter) = 1 then
             add 1 to neighbours
           end-if
           if old-columns(row-counter + 1, column-counter - 1) = 1 then
             add 1 to neighbours
           end-if

           if old-columns(row-counter, column-counter + 1) = 1 then
             add 1 to neighbours
           end-if
           if old-columns(row-counter, column-counter - 1) = 1 then
             add 1 to neighbours
           end-if

           if neighbours < 2 then
             move 0 to new-columns(row-counter, column-counter)
           end-if
           if neighbours = 2 or neighbours = 3 and
             old-columns(row-counter, column-counter) = 1 then
             move 1 to new-columns(row-counter, column-counter)
           end-if
           if old-columns(row-counter, column-counter) = 0 and
             neighbours = 3 then
             move 1 to new-columns(row-counter, column-counter)
           end-if
           if neighbours > 3 then
             move 0 to new-columns(row-counter, column-counter)
           end-if.
