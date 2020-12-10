with Ada.Text_IO;

procedure Day9 is

   subtype Input_Size_T is Positive range 1 .. 2000;

   type Input_Data_T is Array (Input_Size_T) of Long_Long_Integer;

   Input_Data : Input_Data_T;
   Input_Size : Natural := 0;

   Preamble_Size : constant Natural := 25;

   Invalid_Number : Long_Long_Integer := 0;

   function Check_Data 
      (Input_Data : Input_Data_T;
       Input_Size : Natural)
       return Boolean
   is
   begin
      for I in Input_Size_T range Input_Size - Preamble_Size .. Input_Size loop
         for J in Input_Size_T range I + 1 .. Input_Size loop
            if Input_Data (I) + Input_Data (J) = Input_Data (Input_Size) then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Check_Data;

   procedure Find_Weakness 
      (Input_Data     : Input_Data_T;
       Input_Size     : Natural;
       Invalid_Number : Long_Long_Integer)
   is
      Current_Sum : Long_Long_Integer;
      Smallest : Long_Long_Integer := Long_Long_Integer'Last;
      Largest  : Long_Long_Integer := Long_Long_Integer'First;
      Match_Number : constant Long_Long_Integer := Invalid_Number;
   begin
      for I in Input_Size_T range 1 .. Input_Size loop
         Current_Sum := Input_Data (I);

         Inner_Loop: 
         for J in Input_Size_T range I + 1 .. Input_Size loop

            Current_Sum := Current_Sum + Input_Data (J);
            if Current_Sum = Match_Number then

               for K in Input_Size_T range I .. J loop
                  if Input_Data (K) < Smallest then
                     Smallest := Input_Data (K);
                  end if;
                  if Input_Data (K) > Largest then
                     Largest := Input_Data (K);
                  end if;
               end loop;

               Ada.Text_IO.Put_Line ("S/L" & Long_Long_Integer (Smallest + Largest)'Img);
               exit;

            elsif Current_Sum > Match_Number then
               exit Inner_Loop;
            end if;
         end loop Inner_Loop;
      end loop;
   end Find_Weakness;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Input_Size := Input_Size + 1;
         Input_Data (Input_Size) := Long_Long_Integer'Value (Line);
         if Input_Size > Preamble_Size then
            if not Check_Data (Input_Data, Input_Size)
            then
               Ada.Text_IO.Put_Line ("Invalid: " & Line);
               Invalid_Number := Input_Data (Input_Size);
            end if;
         end if;
      end;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   Find_Weakness (Input_Data, Input_Size, Invalid_Number);

end Day9;
