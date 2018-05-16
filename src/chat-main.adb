with Ada.Text_IO;
with Ada.Command_Line;
with DDS;

procedure Chat.Main is

   use Ada.Text_IO;
   use DDS;
   use Ada.Command_Line;
   use Chat;
   arg : CommandLineArguments;
   i   : Natural;

begin

   arg.domainId := 0;
   arg.mode := APP_MODE_UNDEFINED;

   Put_Line ("WELCOME TO THE CHAT");
   Put_Line ("");

   if Argument_Count < 1 then
      Put_Line ("! Invalid number of arguments.");
      Put_Line ("! You must specify at least running mode (pub/sub)");
      return;
   end if;

   if Argument (1) = "pub" then
      arg.mode := APP_MODE_PUBLISHER;
   elsif Argument (1) = "sub" then
      arg.mode := APP_MODE_SUBSCRIBER;
   elsif Argument (1) /= "-h" and Argument (1) /= "--help" then
      usage;
      return;
   else
      usage;
      Put_Line ("! Invalid mode: '" & Argument (1) & "'");
      Put_Line ("! Valid modes are only 'pub' or 'sub'");
      return;
   end if;

   i := 2;
   while i <= Argument_Count loop
      if Argument (i) = "-h" or Argument (i) = "--help" then
         usage;
         return;
      elsif Argument (i) = "-d" or Argument (i) = "--domain" then
         if i + 1 > Argument_Count then
            usage;
            Put_Line ("<domainID> parameter missing");
            return;
         end if;

         if Integer'Value (Argument (i + 1)) > DOMAIN_ID_MAX or Integer'Value (Argument (i + 1)) < 0 then
            Put_Line ("! Invalid DDS Domain ID: " & arg.domainId'Img);
            Put_Line ("! The domain ID must be between 0 and " &
                        DOMAIN_ID_MAX'Img & " (inclusive)");
            return;
         else
            arg.domainId := DDS.DomainId_T'Value (Argument (i + 1));
         end if;
         i := i + 1;
      else
         usage;
         Put_Line ("! Unknown argument " & Argument (i));
      end if;
      i := i + 1;
   end loop;

   startApplication (arg);

end Chat.Main;
