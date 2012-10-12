with Ada.Unchecked_Deallocation;
 
package body Queue_FIFO is 
 
   protected body QueueFIFO_Type is
   
      procedure Push (Item : in Element_Type) is
         Temp : Fifo_Ptr := new Fifo_Element'(Item, null);
      begin
         if Tail = null then
            Tail := Temp;
         end if;
         if Head /= null then
           Head.all.Next := Temp;
         end if;
         Head := Temp;
      end Push;
    
      entry Pop (Item : out Element_Type) when Head /= null is
         procedure Free is new Ada.Unchecked_Deallocation(Fifo_Element, Fifo_Ptr);
         Temp : Fifo_Ptr := Tail;
      begin
         if Head = null then
            raise Empty_Error;
         end if;
         Item := Tail.all.Value;
         Tail := Tail.all.Next;
         if Tail = null then
            Head := null;
         end if;
         Free(Temp);
      end Pop;
 
   end QueueFIFO_Type;
 
end Queue_FIFO;
