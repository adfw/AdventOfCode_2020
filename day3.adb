with Ada.Text_IO;

procedure Day3 is

   Tree     : constant Character := '#';

   Num_Trees_R1D1 : Natural := 0;
   Line_Pos_R1D1  : Positive := 1;

   Num_Trees_R3D1 : Natural := 0;
   Line_Pos_R3D1  : Positive := 1;

   Num_Trees_R5D1 : Natural := 0;
   Line_Pos_R5D1  : Positive := 1;

   Num_Trees_R7D1 : Natural := 0;
   Line_Pos_R7D1  : Positive := 1;

   Num_Trees_R1D2 : Natural := 0;
   Line_Pos_R1D2  : Positive := 1;

   First_Line : Boolean := True;
   Even_Line  : Boolean := True;

   procedure Move
      (Line      : in     String;
       R_Move    : in     Positive;
       Move_Down : in     Boolean;
       Line_Pos  : in out Positive;
       Num_Trees : in out Natural)
   is
   begin
      if not Move_Down then
         if Line_Pos /= Line'Length then
            Line_Pos := Line_Pos rem Line'Length;
         else
            Line_Pos := Line'Length;
         end if;
         if Line (Line_Pos) = Tree then
            Num_Trees := Num_Trees + 1;
         end if;
      end if;
      Line_Pos := Line_Pos + R_Move;
   end Move;
begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Move
          (Line      => Line,
           R_Move    => 1,
           Move_Down => First_Line,
           Line_Pos  => Line_Pos_R1D1,
           Num_Trees => Num_Trees_R1D1);
         Move
          (Line      => Line,
           R_Move    => 3,
           Move_Down => First_Line,
           Line_Pos  => Line_Pos_R3D1,
           Num_Trees => Num_Trees_R3D1);
         Move
          (Line      => Line,
           R_Move    => 5,
           Move_Down => First_Line,
           Line_Pos  => Line_Pos_R5D1,
           Num_Trees => Num_Trees_R5D1);
         Move
          (Line      => Line,
           R_Move    => 7,
           Move_Down => First_Line,
           Line_Pos  => Line_Pos_R7D1,
           Num_Trees => Num_Trees_R7D1);

         if Even_Line then
            Even_Line := False;
            Move
             (Line      => Line,
              R_Move    => 1,
              Move_Down => First_Line,
              Line_Pos  => Line_Pos_R1D2,
              Num_Trees => Num_Trees_R1D2);
         else
            Even_Line := True;
         end if;
         if First_Line then
            First_Line := False;
         end if;

      end;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   Ada.Text_IO.Put_Line("Trees Part 1: " & Num_Trees_R3D1'Img);
   Ada.Text_IO.Put_Line("Trees Part 2: " & Long_Integer 
      (Num_Trees_R1D1 *
       Num_Trees_R3D1 * 
       Num_Trees_R5D1 *
       Num_Trees_R7D1 * 
       Num_Trees_R1D2)'Img);
end Day3;
