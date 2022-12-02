package Day_1 is
   File_Name : constant String := "../inputs/01.txt";
   procedure Part_1;
   procedure Part_2;
private
   type Sliding_Window is array (1 .. 3) of Integer;
   function Depth (Window : in Sliding_Window) return Integer;
end Day_1;
