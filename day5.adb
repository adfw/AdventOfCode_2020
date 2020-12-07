with Ada.Text_IO;

procedure Day5 is

   subtype Seat_Row is Natural range 0 .. 127;

   subtype Seat_Col is Natural range 0 .. 7;
   
   subtype Seat_ID is Natural range 0 .. 1027;

   type Seating_T is Array (Seat_ID) of Boolean;

   Row_Lower  : Seat_Row := Seat_Row'First;
   Row_Upper  : Seat_Row := Seat_Row'Last;
   Row_Select : Seat_Row;

   Col_Lower  : Seat_Col := Seat_Col'First;
   Col_Upper  : Seat_Col := Seat_Col'Last;
   Col_Select : Seat_Col;

   Highest_ID : Natural := 0;
   Seating : Seating_T := (others => False);

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Row_Lower := Seat_Row'First;
         Row_Upper := Seat_Row'Last;
         Col_Lower := Seat_Col'First;
         Col_Upper := Seat_Col'Last;
         for C in Line'Range loop
            case Line (C) is
               when 'B' => 
                  if C /= 7 then
                     Row_Lower := Row_Lower + (2 ** (7 - C));
                  else
                     Row_Select := Row_Upper;
                  end if;
               when 'F' => 
                  if C /= 7 then
                     Row_Upper := Row_Upper - (2 ** (7 - C));
                  else
                     Row_Select := Row_Lower;
                  end if;
               when 'R' => 
                  if C /= 10 then
                     Col_Lower := Col_Lower + (2 ** (10 - C));
                  else
                     Col_Select := Col_Upper;
                  end if;
               when 'L' => 
                  if C /= 10 then
                     Col_Upper := Col_Upper - (2 ** (10 - C));
                  else
                     Col_Select := Col_Lower;
                  end if;
               when others =>
                  null;
            end case;
         end loop;

         Seating (Row_Select * 8 + Col_Select) := True;

         if Highest_ID <  Row_Select * 8 + Col_Select then
            Highest_ID := Row_Select * 8 + Col_Select;
         end if;

      end;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   Ada.Text_IO.Put_Line ("High Seat" & Highest_ID'Img);

   for S in Seat_ID range Seat_ID'First .. Seating_T'Last - 2 loop
      if Seating (S) and then Seating (S + 2) and then not Seating (S + 1) then
         Ada.Text_IO.Put_Line ("SEAT" & Natural(S + 1)'Img);
      end if;
   end loop;

end Day5;
