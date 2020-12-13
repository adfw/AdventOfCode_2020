with Ada.Text_IO;

procedure Day11 is

   type Seat is
      (Empty,
       Occupied,
       Floor,
       Invalid);

   type Char_To_Seat_T is Array (Character) of Seat;

   Char_To_Seat : constant Char_To_Seat_T := Char_To_Seat_T'
    ('L'    => Empty,
     '#'    => Occupied,
     '.'    => Floor,
     others => Invalid);

   type Seat_To_Char_T is Array (Seat) of Character;

   Seat_To_Char : constant Seat_To_Char_T := Seat_To_Char_T'
    (Empty => 'L',
     Occupied => '#',
     Floor    => '.',
     Invalid  => 'X');

   subtype Row_Count_T is Natural range 0 .. 100;
   subtype Row_Num_T is Row_Count_T range 1 .. Row_Count_T'Last;

   subtype Col_Count_T is Natural range 0 .. 100;
   subtype Col_Num_T is Col_Count_T range 1 .. Col_Count_T'Last;

   type Seating_X_T is Array (Col_Num_T) of Seat;

   Empty_Row : constant Seating_X_T := (others => Invalid);

   type Seating_X_Y_T is Array (Row_Num_T) of Seating_X_T;

   Empty_Grid : constant Seating_X_Y_T := (others => Empty_Row);

   type Seating_T is record
      Seats     : Seating_X_Y_T;
      Col_Count : Col_Count_T;
      Row_Count : Row_Count_T;
   end record;

   Empty_Seating : constant Seating_T := Seating_T'
      (Seats     => Empty_Grid,
       Col_Count => 0,
       Row_Count => 0);

   Seating : Seating_T := Empty_Seating;
   New_Seating : Seating_T := Empty_Seating;
   Old_Seating : Seating_T := Empty_Seating;


   Occupied_Seats : Natural := 0;

   function Is_Empty_Or_Occupied
     (Seating : Seating_T;
      Row     : Row_Count_T;
      Col     : Col_Count_T)
  return Boolean is (Seating.Seats (Row) (Col) in Empty .. Occupied);

   function Is_Occupied 
     (Seating : Seating_T;
      Row     : Row_Count_T;
      Col     : Col_Count_T)
  return Boolean is (Seating.Seats (Row) (Col) = Occupied);

  function Num_Occupied
     (Seating : Seating_T;
      Row     : Row_Count_T;
      Col     : Col_Count_T)
      return Natural
  is
     Adjacent_Count : Natural := 0;
  begin
      -- Test adjacent positions.
      -- 1 2 3
      -- 4 X 5
      -- 6 7 8 
      if Row > Row_Num_T'First then
         if Col > Col_Num_T'First then
            if Is_Occupied (Seating, Row - 1, Col - 1) then -- Position 1
               Adjacent_Count := Adjacent_Count + 1;
            end if;
         end if;
         if Is_Occupied (Seating, Row - 1, Col) then -- Position 2
            Adjacent_Count := Adjacent_Count + 1;
         end if;
         if Col < Seating.Col_Count then
            if Is_Occupied (Seating, Row - 1, Col + 1) then -- Position 3
               Adjacent_Count := Adjacent_Count + 1;
            end if;
         end if;
      end if;
      if Col > Col_Num_T'First then
         if Is_Occupied (Seating, Row, Col - 1) then -- Position 4
            Adjacent_Count := Adjacent_Count + 1;
         end if;
      end if;
      if Col < Seating.Col_Count then
         if Is_Occupied (Seating, Row, Col + 1) then -- Position 5
            Adjacent_Count := Adjacent_Count + 1;
         end if;
      end if;
      if Row < Seating.Row_Count then
         if Col > Col_Num_T'First then
            if Is_Occupied (Seating, Row + 1, Col - 1) then -- Position 6
               Adjacent_Count := Adjacent_Count + 1;
            end if;
         end if;
         if Is_Occupied (Seating, Row + 1, Col) then -- Position 7
            Adjacent_Count := Adjacent_Count + 1;
         end if;
         if Col < Seating.Col_Count then
            if Is_Occupied (Seating, Row + 1, Col + 1) then -- Position 8
               Adjacent_Count := Adjacent_Count + 1;
            end if;
         end if;
      end if;
      return Adjacent_Count;
  end Num_Occupied;

   procedure Seat_Sim_1
      (Seating     : in     Seating_T;
       New_Seating :    out Seating_T)
   is
      Adjacent_Count : Natural := 0;

   begin
      New_Seating.Row_Count := Seating.Row_Count;
      New_Seating.Col_Count := Seating.Col_Count;

      for Row in Row_Num_T'First .. Seating.Row_Count loop
         for Col in Col_Num_T'First .. Seating.Col_Count loop
            -- Ada.Text_IO.Put (Seat_To_Char(Seating.Seats (Row) (Col)));

            Adjacent_Count := Num_Occupied (Seating, Row, Col);
               
            case Seating.Seats (Row) (Col) is
               when Floor | Invalid => 
                  New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
               when Empty =>
                  if Adjacent_Count = 0 then
                     New_Seating.Seats (Row) (Col) := Occupied;
                  else
                     New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
                  end if;
               when Occupied => 
                  if Adjacent_Count >= 4 then
                     New_Seating.Seats (Row) (Col) := Empty;
                  else
                     New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
                  end if;
            end case;
         end loop;
         -- ada.Text_IO.Put_Line ("");
      end loop;
   end Seat_Sim_1;

   procedure Seat_Sim_2
      (Seating     : in     Seating_T;
       New_Seating :    out Seating_T)
   is
      Adjacent_Count : Natural := 0;

      Row_Limit : Natural := 0;
      Col_Limit : Natural := 0;
      The_Limit : Natural := 0;

   begin
      New_Seating.Row_Count := Seating.Row_Count;
      New_Seating.Col_Count := Seating.Col_Count;

      for Row in Row_Num_T'First .. Seating.Row_Count loop
         for Col in Col_Num_T'First .. Seating.Col_Count loop
            -- Ada.Text_IO.Put (Seat_To_Char(Seating.Seats (Row) (Col)));
            Adjacent_Count := 0;
            -- Test positions in a particular direction until we reach a seat.
            -- 1 2 3
            -- 4 X 5
            -- 6 7 8 
            if Row > Row_Num_T'First then
               if Col > Col_Num_T'First then
                  -- Find limits
                  Row_Limit := Row - Row_Num_T'First;
                  Col_Limit := Col - Col_Num_T'First;
                  if Row_Limit < Col_Limit then
                     The_Limit := Row_Limit;
                  else
                     The_Limit := Col_Limit;
                  end if;

                  for XY in 1 .. The_Limit loop
                     if Is_Occupied (Seating, Row - XY, Col - XY) then -- Position 1
                        Adjacent_Count := Adjacent_Count + 1;
                        exit;
                     end if;
                     exit when Is_Empty_Or_Occupied (Seating, Row - XY, Col - XY);
                  end loop;
               end if;
               for Y in 1 .. (Row - Row_Num_T'First) loop
                  if Is_Occupied (Seating, Row - Y, Col) then -- Position 2
                     Adjacent_Count := Adjacent_Count + 1;
                     exit;
                  end if;
                  exit when Is_Empty_Or_Occupied (Seating, Row - Y, Col);
               end loop;
               if Col < Seating.Col_Count then
                  Row_Limit := Row - Row_Num_T'First;
                  Col_Limit := Seating.Col_Count - Col;
                  if Row_Limit < Col_Limit then
                     The_Limit := Row_Limit;
                  else
                     The_Limit := Col_Limit;
                  end if;
                 
                  for XY in 1 .. The_Limit loop 
                     if Is_Occupied (Seating, Row - XY, Col + XY) then -- Position 3
                        Adjacent_Count := Adjacent_Count + 1;
                        exit;
                     end if;
                     exit when Is_Empty_Or_Occupied (Seating, Row - XY, Col + XY);
                  end loop;
               end if;
            end if;
            if Col > Col_Num_T'First then
               for Y in 1 .. Col - Col_Num_T'First loop
                  if Is_Occupied (Seating, Row, Col - Y) then -- Position 4
                     Adjacent_Count := Adjacent_Count + 1;
                     exit;
                  end if;
                  exit when Is_Empty_Or_Occupied (Seating, Row, Col - Y);
               end loop;
            end if;
            if Col < Seating.Col_Count then
               for Y in 1 .. Seating.Col_Count - Col loop
                  if Is_Occupied (Seating, Row, Col + Y) then -- Position 5
                     Adjacent_Count := Adjacent_Count + 1;
                     exit;
                  end if;
                  exit when Is_Empty_Or_Occupied (Seating, Row, Col + Y);
               end loop;
            end if;
            if Row < Seating.Row_Count then
               if Col > Col_Num_T'First then
                  Row_Limit := Seating.Row_Count - Row;
                  Col_Limit := Col - Col_Num_T'First;
                  if Row_Limit < Col_Limit then
                     The_Limit := Row_Limit;
                  else
                     The_Limit := Col_Limit;
                  end if;
                  for XY in 1 .. The_Limit loop
                     if Is_Occupied (Seating, Row + XY, Col - XY) then -- Position 6
                        Adjacent_Count := Adjacent_Count + 1;
                        exit;
                     end if;
                     exit when Is_Empty_Or_Occupied (Seating, Row + XY, Col - XY);
                  end loop;
               end if;
               for X in 1 .. Seating.Row_Count - Row loop
                  if Is_Occupied (Seating, Row + X, Col) then -- Position 7
                     Adjacent_Count := Adjacent_Count + 1;
                     exit;
                  end if;
                  exit when Is_Empty_Or_Occupied (Seating, Row + X, Col);
               end loop;
               if Col < Seating.Col_Count then
                  Row_Limit := Seating.Row_Count - Row;
                  Col_Limit := Seating.Col_Count - Col;
                  if Row_Limit < Col_Limit then
                     The_Limit := Row_Limit;
                  else
                     The_Limit := Col_Limit;
                  end if;
                  for XY in 1 .. The_Limit loop
                     if Is_Occupied (Seating, Row + XY, Col + XY) then -- Position 8
                        Adjacent_Count := Adjacent_Count + 1;
                        exit;
                     end if;
                     exit when Is_Empty_Or_Occupied (Seating, Row + XY, Col + XY);
                  end loop;
               end if;
            end if;
            -- Ada.Text_IO.Put(Adjacent_Count'Img);
               
            case Seating.Seats (Row) (Col) is
               when Floor | Invalid => 
                  New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
               when Empty =>
                  if Adjacent_Count = 0 then
                     New_Seating.Seats (Row) (Col) := Occupied;
                  else
                     New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
                  end if;
               when Occupied => 
                  if Adjacent_Count >= 5 then
                     New_Seating.Seats (Row) (Col) := Empty;
                  else
                     New_Seating.Seats (Row) (Col) := Seating.Seats (Row) (Col);
                  end if;
            end case;
         end loop;
         -- ada.Text_IO.Put_Line ("");
      end loop;
   end Seat_Sim_2;
begin

   Seating.Row_Count := Seating.Row_Count + 1;
   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Seating.Col_Count := 0;
         for C in Line'Range loop
            Seating.Col_Count := Seating.Col_Count + 1;
            Seating.Seats (Seating.Row_Count) (Seating.Col_Count) := Char_To_Seat (Line (C));
         end loop;
      end;
      exit when Ada.Text_IO.End_Of_File;
      Seating.Row_Count := Seating.Row_Count + 1;
   end loop;

   Old_Seating := Seating;

   Ada.Text_IO.Put_Line ("Read" & Seating.Row_Count'Img & Seating.Col_Count'Img);

   loop
      Seat_Sim_1 (Seating, New_Seating);
      exit when Seating.Seats = New_Seating.Seats;
      Seating := New_Seating;
   end loop;

   for Row in Row_Num_T'First .. Seating.Row_Count loop
      for Col in Col_Num_T'First .. Seating.Col_Count loop
         if Is_Occupied (Seating, Row, Col) then
            Occupied_Seats := Occupied_Seats + 1;
         end if;
      end loop;
   end loop;

  Ada.Text_IO.Put_Line ("Occupied: " & Occupied_Seats'Img); 
  Seating := Old_Seating;

   loop
      Seat_Sim_2 (Seating, New_Seating);
      exit when Seating.Seats = New_Seating.Seats;
      Seating := New_Seating;
   end loop;

   Occupied_Seats := 0;
   for Row in Row_Num_T'First .. Seating.Row_Count loop
      for Col in Col_Num_T'First .. Seating.Col_Count loop
         if Is_Occupied (Seating, Row, Col) then
            Occupied_Seats := Occupied_Seats + 1;
         end if;
      end loop;
   end loop;

  Ada.Text_IO.Put_Line ("Occupied: " & Occupied_Seats'Img); 

end Day11;
