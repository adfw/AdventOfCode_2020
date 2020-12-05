with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;


procedure Day2 is

   Min_Length : Natural;
   Max_Length : Natural;

   Line_Pos : Natural;

   Check_Char : Character;

   N_Of_Char : Natural;

   N_Valid_Part1 : Natural := 0;
   N_Valid_Part2 : Natural := 0;
begin

   loop
      declare
         Line : String := Ada.Text_IO.Get_Line;
      begin
         Ada.Integer_Text_IO.Get(Line, Min_Length, Line_Pos);
         Line_Pos := Line_Pos + 1; -- Skip over the '-'
         Ada.Strings.Fixed.Delete (Line, 1, Line_Pos);
         Ada.Integer_Text_IO.Get(Line, Max_Length, Line_Pos);
         Line_Pos := Line_Pos + 2; -- Skip over the ' '
         Check_Char := Line (Line_Pos);
         Ada.Strings.Fixed.Delete (Line, 1, Line_Pos + 2); -- Skip over the 'x: '

         N_Of_Char := 0;

         for Char in Line'Range loop
            if Line (Char) = Check_Char then
               N_Of_Char := N_Of_Char +1;
            end if;
         end loop;

         if N_Of_Char >= Min_Length and then
            N_Of_Char <= Max_Length 
         then
            N_Valid_Part1 := N_Valid_Part1 + 1;
         end if;

         if (Line (Min_Length) = Check_Char and then
            Line (Max_Length) /= Check_Char) or else 
            (Line (Min_Length) /= Check_Char and then
            Line (Max_Length) = Check_Char)
         then
            N_Valid_Part2 := N_Valid_Part2 + 1;
         end if;
      end;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   Ada.Text_IO.Put_Line("Valid 1: " & N_Valid_Part1'Img);
   Ada.Text_IO.Put_Line("Valid 2: " & N_Valid_Part2'Img);

end Day2;

