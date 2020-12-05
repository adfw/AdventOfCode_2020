with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day4 is

   type Field is
      (byr,
       iyr,
       eyr,
       hgt,
       hcl,
       ecl,
       pid,
       cid);

   package Field_IO is new Ada.Text_IO.Enumeration_IO (Field);

   type Field_Present_T is Array (Field) of Boolean;

   Null_Field_Present_C : constant Field_Present_T := 
      (others => False);

   Valid_Passport_C : constant Field_Present_T :=
      (others => True);

   Valid_Passport_CID_C : constant Field_Present_T := 
      ( cid    => False,
        others => True);

   Token : Field;

   Line_Pos : Natural;
   Col : Natural := 0;

   Field_Present_P1 : Field_Present_T := Null_Field_Present_C;
   Field_Present_P2 : Field_Present_T := Null_Field_Present_C;

   Valid_Count_P1 : Natural := 0;
   Valid_Count_P2 : Natural := 0;

   function Check_Value
      (Line  : String;
       Token : Field)
       return Boolean   
   is
      Trim_Line : String := Line;

      Line_Pos : Natural := 0;
      Value : Natural;

      type Eye_Colour is
         (amb, blu, brn, gry, grn, hzl, oth);

      package Eye_O is new Ada.Text_IO.Enumeration_IO (Eye_Colour);

      Eye : Eye_Colour;

      subtype Byr_Valid is Positive range 1920 .. 2002;
      subtype Iyr_Valid is Positive range 2010 .. 2020;
      subtype Eyr_Valid is Positive range 2020 .. 2030;
      subtype Height_CM_Valid is Positive range 150 .. 193;
      subtype Height_IN_Valid is Positive range 59 .. 76;
      subtype PID_Valid is Natural range 0 .. 999999999;

      Result : Boolean := False;

   begin

      Ada.Strings.Fixed.Delete (Trim_Line, 1, 4);

      case Token is

         when Byr => 

            Ada.Integer_Text_IO.Get(Trim_Line, Value, Line_Pos);

            if Value in Byr_Valid then
               Result := True;
            end if;

         when Iyr => 

            Ada.Integer_Text_IO.Get(Trim_Line, Value, Line_Pos);

            if Value in Iyr_Valid then
               Result := True;
            end if;

         when Eyr => 

            Ada.Integer_Text_IO.Get(Trim_Line, Value, Line_Pos);

            if Value in Eyr_Valid then
               Result := True;
            end if;

         when Hgt =>

            Ada.Integer_Text_IO.Get(Trim_Line, Value, Line_Pos);
            Line_Pos := Ada.Strings.Fixed.Index(Line, "cm");
            if Line_Pos /= 0 and then Value in Height_CM_Valid then
               Result := True;
            end if;
            Line_Pos := Ada.Strings.Fixed.Index(Line, "in");
            if Line_Pos /= 0 and then Value in Height_IN_Valid then
               Result := True;
            end if;

         when Hcl => 

            if Trim_Line (1) = '#' and then Trim_Line'Length >= 7 then

               for I in 2 .. 7 loop
                  if Trim_Line (I) not in '0' .. '9' and then 
                     Trim_Line (I) not in 'a' .. 'f'
                  then
                     exit;
                  end if;

                  -- if we get here, then it was valid.
                  Result := True;
               end loop;
            end if;


         when Ecl => 

            Eye_O.Get (Trim_Line, Eye, Line_Pos);

            if Eye'Valid then
               -- This is guarded by the exception anyway, but I might get rid of it.
               Result := True;
            end if;

         when Pid => 

            Ada.Integer_Text_IO.Get(Trim_Line, Value, Line_Pos);

            if Value in PID_Valid and then Line_Pos = 9 then
               Result := True;
            end if;

         when Cid => 

            null; 

      end case;

      return Result;

   exception 
      when E : Ada.IO_Exceptions.Data_Error => 
         return False;
   end Check_Value;

begin

   loop
      declare
         Line : String := Ada.Text_IO.Get_Line;
      begin
         if Line'Length > 0 then
            Col := 0;
            while Col < Line'Length  loop
               Field_IO.Get(Line, Token, Line_Pos);
               Field_Present_P1 (Token) := True;
               if Check_Value (Line, Token) then
                  Field_Present_P2 (Token) := True;
               end if;
               Line_Pos := Ada.Strings.Fixed.Index(Line, " ", Line_Pos);
               Ada.Strings.Fixed.Delete (Line, 1, Line_Pos);
               Col := Col + Line_Pos;
               exit when Line_Pos = 0;
            end loop;   
         elsif Line'Length = 0 then
            -- End of record - check status
            if Field_Present_P1 = Valid_Passport_C or else
               Field_Present_P1 = Valid_Passport_CID_C then
               Valid_Count_P1 := Valid_Count_P1 + 1;
            end if;
            if Field_Present_P2 = Valid_Passport_C or else
               Field_Present_P2 = Valid_Passport_CID_C then
               Valid_Count_P2 := Valid_Count_P2 + 1;
            end if;
            Field_Present_P1 := Null_Field_Present_C;
            Field_Present_P2 := Null_Field_Present_C;
         end if;
      end;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   -- Check final line.
   if Field_Present_P1 = Valid_Passport_C or else
      Field_Present_P1 = Valid_Passport_CID_C then
      Valid_Count_P1 := Valid_Count_P1 + 1;
   end if;
   if Field_Present_P2 = Valid_Passport_C or else
      Field_Present_P2 = Valid_Passport_CID_C then
      Valid_Count_P2 := Valid_Count_P2 + 1;
   end if;

   Ada.Text_IO.Put_Line("Valid Part 1: " & Valid_Count_P1'Img);
   Ada.Text_IO.Put_Line("Valid Part 2: " & Valid_Count_P2'Img);
end Day4;
