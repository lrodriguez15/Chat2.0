with DDS.Publisher;
with DDS.DataWriter;
with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Chat.Publisher is
   use Ada.Text_IO;

   function Init (participant : DDS.DomainParticipant.Ref_Access;
                  topic : DDS.Topic.Ref_Access;
                  nickname : SU.Unbounded_String)

                  return ChatPublisher
   is

      data_writer   : DDS.DataWriter.Ref_Access                 := null;
      NULL_ENTITY   : exception;

      use type DDS.DomainParticipant.Ref_Access;
      use type DDS.Topic.Ref_Access;
      use type DDS.DataWriter.Ref_Access;
      use type DDS.Publisher.Ref_Access;

   begin

      return ret : ChatPublisher do

         Put_Line ("Ready to chat");
         Put_Line ("Make sure to open a subscriber terminal. You can start writing!.");
         Put_Line ("Press CTRL+C to terminate or enter ""#EXIT"" for a clean shutdown.");
         --  THIS IS A PENDING FUNCTION
         --  Put_Line ("If You want to submit a message to other room please write ""#Thenameoftheroom"".");
         --  Put_Line ("at the beginning of the message");
         Put_Line ("");
         Put_Line ("YOU ARE IN " & DDS.To_Standard_String (topic.Get_Name) & " ROOM");

         data_writer := participant.Create_DataWriter -- CREATE THE WRITER
           (A_Topic    => topic,
            Qos        => DDS.Publisher.DATAWRITER_QOS_DEFAULT,
            A_Listener => null,
            Mask       => DDS.STATUS_MASK_NONE);
         if data_writer = null then
            raise NULL_ENTITY with "Unable to create datawriter";
         end if;

         ret.writer := DDS.Builtin_String_DataWriter.Narrow (data_writer);
         ret.nickname := nickname;
         ret.topic := topic;
      end return;

   end Init;

   procedure Publish (pub : ChatPublisher) is

      now   : constant Time := Clock;
      welcome_message : constant String := SU.To_String (pub.nickname) & " joined the chat at " &
      Image (Date => Now, Time_Zone => +1 * 60);
      ddsWelcome : constant DDS.String := DDS.To_DDS_STRING (welcome_message);
      handlewelcome  : aliased DDS.InstanceHandle_T;

   begin
         Put_Line ("Joining...");
         delay DELAY_TIME;
         handlewelcome := DDS.HANDLE_NIL;
         pub.writer.Write (ddsWelcome, handlewelcome'Unchecked_Access);
      loop
         Put ("Please type a message> ");
         declare
            message : constant String := Get_Line;
            full : constant String := SU.To_String (pub.nickname) & "> " & message;
         begin
            exit when message = "#EXIT";

            --  if Index ("#SPORTS", message) > 0 then
            --   Put_Line ("Writing to Sports...(pending function)");
            --  elsif Index ("#BOOKS", message) > 0 then
            --   Put_Line ("Writing to Books...(pending function)");
            --  elsif Index ("#MUSIC", message) > 0 then
            --    Put_Line ("Writing to Music...(pending function)");
            --  end if;

            declare
               ddsMsg  : DDS.String := DDS.To_DDS_String (full);
               handle  : aliased DDS.InstanceHandle_T := DDS.HANDLE_NIL;
            begin
               if Index ("#WHERE", message) > 0 then
                Put_Line (DDS.To_Standard_String (pub.topic.Get_Name));
               else
                pub.writer.Write
                    (Instance_Data => ddsMsg,
                    Handle        => handle'Unchecked_Access);
                DDS.Finalize (ddsMsg);
               end if;
            end;
         end;
      end loop;

   end Publish;

end Chat.Publisher;