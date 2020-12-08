with Ada.Text_IO;

procedure Day6 is

   subtype Questions_T is Character range 'a' .. 'z';

   type Questions is Array (Questions_T) of Boolean;

   Null_Questions : constant Questions := (others => False);

   Q_Sum : Natural := 0;
   Q_All : Natural := 0;

   Line_Questions  : Questions := Null_Questions;
   Group_Questions : Questions := Null_Questions;
   All_Questions   : Questions := Null_Questions;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin

         if Line'Length > 0 then

            for Q in Line'Range loop
               if not Group_Questions (Line(Q)) then
                  Q_Sum := Q_Sum + 1;
                  Group_Questions (Line(Q)) := True;
               end if;
                  Line_Questions (Line(Q)) := True;
            end loop;

            if All_Questions = Null_Questions and then
                   Group_Questions = Line_Questions then
               All_Questions := Line_Questions;
            else
               All_Questions := All_Questions and Line_Questions;
            end if;
            Line_Questions := Null_Questions;

         else

            for Q in Questions_T'Range loop
               if All_Questions (Q) then
                  Q_All := Q_All + 1;
               end if;
            end loop;

            -- Empty line, reset questions array
            Group_Questions := Null_Questions;
            All_Questions := Null_Questions;

         end if;
      end;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   -- Final group
   for Q in Questions_T'Range loop
      if All_Questions (Q) then
         Q_All := Q_All + 1;
      end if;
   end loop;

   Ada.Text_IO.Put_Line("Part 1:" & Q_Sum'Img);
   Ada.Text_IO.Put_Line("Part 2:" & Q_All'Img);

end Day6;
