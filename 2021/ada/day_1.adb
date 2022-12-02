with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day_1 is
   procedure Part_1 is
      F          : File_Type;
      Depth      : Integer;
      Prev_Depth : Integer;
      Descent    : Integer := 0;
   begin
      Open (F, In_File, File_Name);

      Get (F, Prev_Depth);
      while not End_Of_File (F) loop
         Get (F, Depth);
         if Depth > Prev_Depth then
            Descent := Descent + 1;
         end if;
         Prev_Depth := Depth;
      end loop;

      Close (F);
      Put_Line ("  Part 1:" & Integer'Image (Descent));
   end Part_1;

   procedure Part_2 is
      F           : File_Type;
      Window      : Sliding_Window;
      Prev_Window : Sliding_Window;
      Descent     : Integer := 0;
   begin
      Open (F, In_File, File_Name);

      for I in Prev_Window'Range loop
         Get (F, Prev_Window (I));
      end loop;

      while not End_Of_File (F) loop
         Window (1) := Prev_Window (2);
         Window (2) := Prev_Window (3);
         Get (F, Window (3));
         if Depth (Window) > Depth (Prev_Window) then
            Descent := Descent + 1;
         end if;
         for I in Window'Range loop
            Prev_Window (I) := Window (I);
         end loop;
      end loop;

      Close (F);
      Put_Line ("  Part 2:" & Integer'Image (Descent));
   end Part_2;

   function Depth (Window : in Sliding_Window) return Integer is
      Result : Integer := 0;
   begin
      for N of Window loop
         Result := Result + N;
      end loop;

      return Result;
   end Depth;
end Day_1;
