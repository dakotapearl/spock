generic
   type Element_Type is private;
package Queue_FIFO is
   Empty_Error : exception;
   type Fifo_Element;
   type Fifo_Ptr is access Fifo_Element;
   protected type QueueFIFO_Type is
      procedure Push(Item : in Element_Type);
      entry Pop(Item : out Element_Type);
   private
      Head : Fifo_Ptr := null;
      Tail : Fifo_Ptr := null;
   end QueueFIFO_Type;
   type Fifo_Element is record
      Value : Element_Type;
      Next  : Fifo_Ptr := null;
   end record;
end Queue_FIFO;
