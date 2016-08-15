with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.Sockets;
with Ada.Characters.Latin_1;
with Ada.Streams;

procedure Main is
   use GNAT.Sockets;
   use Ada.Text_IO;
   use Ada.Characters.Latin_1;

   procedure Get_Line (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out String; P : in out Integer) is
   begin
      loop
         Character'Read (Stream, Item (P));
         if Item (P) = CR then
            P := P + 1;
            Character'Read (Stream, Item (P));
            if Item (P) = LF then
               exit;
            end if;
         else
            P := P + 1;
         end if;
      end loop;
   end;

   Server_Socket   : Socket_Type;
   Client_Socket : Socket_Type;
   Client_Address : Sock_Addr_Type;
   Client_Channel    : Stream_Access;
   Response : String := "HTTP/1.1 200 OK " & CR&LF &
     "Content-Type: text/html; charset=UTF-8" & CR&LF&CR&LF &
     "<!DOCTYPE html><html><head><title>Bye-bye baby bye-bye</title>" &
     "<style>body { background-color: #111 }" &
     "h1 { font-size:4cm; text-align: center; color: black;" &
     " text-shadow: 0 0 2mm red}</style></head>" &
     "<body><h1>Goodbye, world!</h1></body></html>" & CR&LF;

   Request : String (1 .. 1000);
   P : Integer;

begin
   Initialize;

   -- Setup server
   Create_Socket (Server_Socket);
   Set_Socket_Option (Server_Socket, Socket_Level, (Name => Reuse_Address, Enabled => True));
   Bind_Socket (Server_Socket, (Family => Family_Inet, Addr => Any_Inet_Addr, Port => 80));
   Listen_Socket (Server_Socket);

   loop
      Accept_Socket (Server_Socket, Client_Socket, Client_Address);
      Put_Line ("Client connected from " & Image (Client_Address));
      Client_Channel := Stream (Client_Socket);

      String'Write (Client_Channel, Response);
      Shutdown_Socket (Client_Socket, Shut_Write);

      loop
         P := Request'First;
         Get_Line (Client_Channel, Request, P);
         Put (Request (Request'First .. P));
         exit when Request (Request'First .. P) = CR & LF;
      end loop;

      Close_Socket (Client_Socket);
   end loop;

   --Finalize;
end;
