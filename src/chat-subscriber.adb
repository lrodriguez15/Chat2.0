with DDS;
with DDS.Subscriber;
with DDS.DataReader;
with Ada.Text_IO;
with Ada.Exceptions;
with Chat.Listener;

package body Chat.Subscriber is
   use Ada.Text_IO;

   procedure readMessages
     (participant   : DDS.DomainParticipant.Ref_Access;
      topic         : DDS.Topic.Ref_Access
      )
   is
      reader       : DDS.DataReader.Ref_Access := null;
      listener     : aliased Chat.Listener.Ref;

      use type DDS.DomainParticipant.Ref_Access;
      use type DDS.Topic.Ref_Access;
      use type DDS.DataReader.Ref_Access;
      use type DDS.Subscriber.Ref_Access;

   begin
      -- Create the reader --
      reader := participant.Create_DataReader
        (Topic      => topic.As_TopicDescription,
         Qos        => DDS.Subscriber.DATAREADER_QOS_DEFAULT,
         A_Listener => listener'Unchecked_Access,
         Mask       => DDS.DATA_AVAILABLE_STATUS);
      if reader = null then
         raise INIT_FAILURE with "Unable to create datawriter";
      end if;
      -- Sleep During Asynchronous Reception --
      Put_Line ("Welcome to the CHAT Subscriber");
      delay 1.0;
      Put_Line ("The System is Getting ready...");
      Put_Line ("Press CTRL+C to terminate.");
      Put_Line ("You are ready to receive messages");
      -- Forever loop until something is received --
      loop
         delay 1.0;
         exit when not listener.receiving;
      end loop;
      delay 1.0;

      Put_Line ("Exiting...");

      if reader /= null then
         participant.Delete_DataReader (reader);
         reader := null;
      end if;

   exception
      when e : others =>
         if reader /= null then
            participant.Delete_DataReader (reader);
            reader := null;
         end if;

         Put_Line (Ada.Exceptions.Exception_Message (e));


   end readMessages;

end Chat.Subscriber;
