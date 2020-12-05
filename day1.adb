with Ada.Text_IO;

procedure Day1
is
   subtype Expenses_Size_T is Natural range 0 .. 1000;
   subtype Expenses_Length_T is Expenses_Size_T range 1 .. Expenses_Size_T'Last;

   type Expenses_List_T is array (Expenses_Length_T) of Natural; -- assume this is big enough.

   type Expenses_T is record
      Expense : Expenses_List_T;
      Length  : Expenses_Size_T;
   end record;

   Empty_Expenses_C : constant Expenses_T :=
      Expenses_T'
         (Expense => (others => 0),
          Length  => 0);
   
   Expense : Expenses_T := Empty_Expenses_C;

   Line_Val : Natural;

   Found_2 : Boolean := False;
   Found_3 : Boolean := False;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin

         Line_Val := Natural'Value(Line);

         for I in Expenses_Length_T range 1 .. Expense.Length loop
            if Line_Val + Expense.Expense (I) = 2020 then
               -- Part 1 - check for 2-sum to 2020
               Ada.Text_IO.Put_Line("Sum 2" & 
                  Natural(Line_Val * Expense.Expense(I))'Img);
               Found_2 := True;
            elsif Line_Val + Expense.Expense (I) < 2020 then
               -- Part 2 - check for 3-sum to 2020
               for J in Expenses_Length_T range I .. Expense.Length loop
                  if Line_Val + Expense.Expense (I) + Expense.Expense (J) = 2020 then
                     Ada.Text_IO.Put_Line("Sum 3" &
                        Natural(Line_Val * Expense.Expense(I) * Expense.Expense(J))'Img);
                     Found_3 := True;
                  end if;
               end loop;
            end if;

            exit when Found_2 and Found_3;

         end loop;

         Expense.Length := Expense.Length + 1;
         Expense.Expense (Expense.Length) := Line_Val;

         exit when Ada.Text_IO.End_Of_File;

      end;

   end loop;

end Day1;
