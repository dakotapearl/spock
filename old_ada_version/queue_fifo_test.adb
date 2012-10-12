with queue_fifo;
with Ada.Text_IO; use Ada.Text_IO;

procedure queue_fifo_test is
   package FIFO is new Queue_FIFO(Integer);
   q : FIFO.QueueFIFO_Type;
   s : Integer;
begin
   q.push(1);
   q.pop(s);
   put_line(Integer'Image(s));
   
   q.push(2);
   q.push(3);
   q.pop(s);
   put_line(Integer'Image(s));
   q.pop(s);
   put_line(Integer'Image(s));
   
   q.push(4);
   q.push(5);
   q.pop(s);
   put_line(Integer'Image(s));
   q.push(6);
   q.pop(s);
   put_line(Integer'Image(s));
   q.pop(s);
   put_line(Integer'Image(s));
   
   q.pop(s);
   
end queue_fifo_test;
