with Ada.Text_IO;

procedure Day25 is

   Card_PK : Long_Long_Integer;
   Door_PK : Long_Long_Integer;

   Subject_Number : constant := 7;
   Rem_Value : constant := 20201227;

   Value : Long_Long_Integer := 1;
   Loop_Number : Natural := 0;

begin

   loop
      declare
         Line_1 : constant String := Ada.Text_IO.Get_Line;
         Line_2 : constant String := Ada.Text_IO.Get_Line;
      begin
         Card_PK := Long_Long_Integer'Value (Line_1);
         Door_PK := Long_Long_Integer'Value (Line_2);

         while Value /= Door_PK loop
            Value := Value * Subject_Number;
            Value := Value rem Rem_Value;
            Loop_Number := Loop_Number + 1;
         end loop;
      end;
      exit when Ada.Text_IO.End_Of_File;
   end loop;

   Value := 1;
   for L in Positive range 1 .. Loop_Number loop
      Value := Value * Card_PK;
      Value := Value rem Rem_Value;
   end loop;
   Ada.Text_IO.Put_Line ("Value : " & Value'Img);

end Day25;
