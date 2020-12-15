with Ada.Text_IO;

procedure Day12 is

   type Action_T is
      (N, E, S, W, L, R, F);

   subtype Direction_T is Action_T range N .. W;

   subtype Rotation_T is Action_T range L .. R;

   type Position_T is record
      X   : Integer;
      Y   : Integer;
      Dir : Direction_T;
   end record;

   Zero_Position : constant Position_T := (0, 0, E);

   Position : Position_T := Zero_Position;

   Action : Action_T;
   Units : Integer;

   Position_P2 : Position_T := Zero_Position;
   Waypoint    : Position_T := (10, 1, E);

   procedure Move
      (Position  : in out Position_T;
       Direction : in     Direction_T;
       Amount    : in     Integer)
   is
   begin
      case Direction is 
         when N => 
            Position.Y := Position.Y + Amount;
         when S =>
            Position.Y := Position.Y - Amount;
         when E => 
            Position.X := Position.X + Amount;
         when W => 
            Position.X := Position.X - Amount;
      end case;
   end Move;

   procedure Rotate
      (Position  : in out Position_T;
       Rotation  : in     Rotation_T;
       Amount    : in     Integer)
   is
      Rotation_Amount : constant Integer := Amount / 90;
   begin
      case Rotation is 
         when L => 
            Position.Dir := Direction_T'Val ((Direction_T'Pos (Position.Dir) - Rotation_Amount) mod 4);
         when R => 
            Position.Dir := Direction_T'Val ((Direction_T'Pos (Position.Dir) + Rotation_Amount) mod 4);
      end case;
   end Rotate;

   procedure Rotate_2
      (Position  : in out Position_T;
       Rotation  : in     Rotation_T;
       Amount    : in     Integer)
   is
      Initial_Position : constant Position_T := Position;
   begin
      case Rotation is 
         when L => 
            if Amount = 90 then
               Position.X := -Initial_Position.Y;
               Position.Y := Initial_Position.X;
            elsif Amount = 180 then
               Position.X := -Initial_Position.X;
               Position.Y := -Initial_Position.Y;
            elsif Amount = 270 then
               Position.X := Initial_Position.Y;
               Position.Y := -Initial_Position.X;
            end if;
         when R => 
            if Amount = 90 then
               Position.X := Initial_Position.Y;
               Position.Y := -Initial_Position.X;
            elsif Amount = 180 then
               Position.X := -Initial_Position.X;
               Position.Y := -Initial_Position.Y;
            elsif Amount = 270 then
               Position.X := -Initial_Position.Y;
               Position.Y := Initial_Position.X;
            end if;
      end case;
   end Rotate_2;

   procedure Move_2
      (Position  : in out Position_T;
       Waypoint  : in     Position_T;
       Amount    : in     Integer)
   is
   begin
      Position.X := Position.X + (Amount * Waypoint.X);
      Position.Y := Position.Y + (Amount * Waypoint.Y);
   end Move_2;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Action := Action_T'Value (Line (1..1));
         Units  := Integer'Value  (Line (2 .. Line'Length));

         case Action is
            when N .. W => 
               Move -- Part 1
                  (Position  => Position,
                   Direction => Action,
                   Amount    => Units);
               Move -- Part 2 
                  (Position  => Waypoint,
                   Direction => Action,
                   Amount    => Units);
            when L .. R => 
               Rotate -- Part 1
                  (Position  => Position,
                   Rotation  => Action,
                   Amount    => Units);
               Rotate_2 -- Part 2
                  (Position  => Waypoint,
                   Rotation  => Action,
                   Amount    => Units);
            when F => 
               Move -- Part 1
                  (Position  => Position,
                   Direction => Position.Dir,
                   Amount    => Units);
               Move_2 -- Part 2
                  (Position  => Position_P2,
                   Waypoint  => Waypoint,
                   Amount    => Units);
         end case;
      end;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   Ada.Text_IO.Put_Line ("Manhattan: " & Integer (abs (Position.X) + abs (Position.Y))'Img);
   Ada.Text_IO.Put_Line ("Manhattan2:" & Integer (abs (Position_P2.X) + abs (Position_P2.Y))'Img);
end Day12;
