with Ada.Text_IO;

procedure Day8 is

   type Op_T is 
      (Acc,
       Jmp,
       Nop);

   type Op_Mapper_T is Array (Op_T) of Op_T;

   Op_Mapper : constant Op_Mapper_T := 
      (Acc => Acc,
       Jmp => Nop,
       Nop => Jmp);

   type Instruction_T is record
      Op  : Op_t;
      Arg : Integer;
   end record;

   subtype Program_Counter_T is Natural range 0 .. 2000;

   subtype PC_Range is Program_Counter_T range 1 .. Program_Counter_T'Last;

   type Program is Array (PC_Range) of Instruction_T;

   type Been_There_T is Array (PC_Range) of Boolean;

   The_Program : Program;

   Program_Counter : Program_Counter_T;

   Accumulator : Integer := 0;
   Terminates : Boolean;

   procedure Test_Program
      (The_Program  : in     Program;
       Program_Size : in     Program_Counter_T;
       Terminates   :    out Boolean;
       Accumulator  :    out Integer)
   is
      Been_There : Been_There_T := (others => False);
      PC : Program_Counter_T := 1;
   begin
      Accumulator := 0;
      loop
         exit when Been_There (PC) or else PC > Program_Size;
         Been_There (PC) := True;
         case The_Program (PC).Op is 
            when Nop => 
               PC := PC + 1;
            when Acc => 
               Accumulator := Accumulator + The_Program (PC).Arg;
               PC := PC + 1;
            when Jmp => 
               PC := PC + The_Program (PC).Arg;
         end case;
      end loop;

      if Been_There (PC) then
         Terminates := False;
      else
         Terminates := True;
      end if;
   end Test_Program;

   procedure Mutate_Program
      (The_Program  : in     Program;
       Program_Size : in     Program_Counter_T;
       Terminates   :    out Boolean;
       Accumulator  :    out Integer)
   is
      Modified_Program : Program;
   begin
      for PC in Program_Counter_T range 1 .. Program_Size loop
         if The_Program (PC).Op in Jmp .. Nop then
            Modified_Program := The_Program;
            Modified_Program (PC).Op := Op_Mapper (The_Program (PC).Op);
            Test_Program
              (The_Program  => Modified_Program,
               Program_Size => Program_Size,
               Terminates   => Terminates,
               Accumulator  => Accumulator);
         end if;
         exit when Terminates;
      end loop;
   end Mutate_Program;
begin

   Program_Counter := 1;
   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         The_Program (Program_Counter) := Instruction_T'
           (Op  => Op_T'Value (Line (1 .. 3)),
            Arg => Integer'Value (Line (5 .. Line'Last)));
         Program_Counter := Program_Counter + 1;
      end;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   Test_Program (
      The_Program,
      Program_Counter,
      Terminates,
      Accumulator);

   Ada.Text_IO.Put_Line ("Accumulator Part 1:" & Accumulator'Img);

   Mutate_Program (
      The_Program,
      Program_Counter,
      Terminates,
      Accumulator);

   Ada.Text_IO.Put_Line ("Accumulator Part 2:" & Accumulator'Img);

end Day8;
