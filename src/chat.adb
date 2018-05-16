with DDS.DomainParticipantFactory;
with DDS.DomainParticipant;
with DDS.Builtin_String_DataWriter;
with DDS.Builtin_String_TypeSupport;
with DDS.Topic;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Chat.Publisher;
with Chat.Subscriber;

package body Chat is
   use Ada.Text_IO;
   package SU   renames Ada.Strings.Unbounded;

   procedure usage is
   begin
      Put_Line ("Usage:");
      Put_Line ("    News pub [arguments]     Run as publisher");
      Put_Line ("    News sub [arguments]     Run as subscriber");
      Put_Line ("Where arguments are:");
      Put_Line ("  -h | --help                   " &
                  "Shows this page");
      Put_Line ("  -d | --domain <domainID>      " &
                  "Sets the DDS domain ID [default=" &
                  DEFAULT_DOMAIN_ID'Img & "]");
      Put_Line ("");
   end usage;

   procedure startApplication (arg : CommandLineArguments) is
      factory       : DDS.DomainParticipantFactory.Ref_Access;
      participant   : DDS.DomainParticipant.Ref_Access;
      topic         : DDS.Topic.Ref_Access;
      NULL_ENTITY   : exception; nickname : SU.Unbounded_String;
      chatroom_name : SU.Unbounded_String;
      room : Chat_Room;
      pub : Chat.Publisher.ChatPublisher;

      use type DDS.DomainParticipant.Ref_Access;
      use type DDS.Topic.Ref_Access;

   begin

      factory := DDS.DomainParticipantFactory.Get_Instance;

      --/* Create the domain participant */

      participant := factory.Create_Participant
        (Domain_Id  => arg.domainId,
         Qos        => DDS.DomainParticipantFactory.PARTICIPANT_QOS_DEFAULT,
         A_Listener => null,
         Mask       => DDS.STATUS_MASK_NONE);
      if participant = null then
         Put_Line ("! Unable to create DomainParticipant");
         raise FAILURE;
      end if;

      --/* Select the chatroom as the topic */

      room := selectChatRoom;

      if room = BOOKS then
         chatroom_name := SU.To_Unbounded_String ("BOOKS");
      elsif room = MUSIC then
         chatroom_name := SU.To_Unbounded_String ("MUSIC");
      elsif room = SPORTS then
         chatroom_name := SU.To_Unbounded_String ("SPORTS");
      else
         raise NULL_ENTITY with "ERROR IN CHATROOM CONFIGURATION";
      end if;

      -- Create the topic --

      topic := participant.Create_Topic
        (Topic_Name => DDS.To_DDS_String (SU.To_String (chatroom_name)),
         Type_Name  => DDS.Builtin_String_TypeSupport.Get_Type_Name,
         Qos        => DDS.DomainParticipant.TOPIC_QOS_DEFAULT,
         A_Listener => null,
         Mask       => DDS.STATUS_MASK_NONE);

      Put_Line ("ACCESS TO CHATROOM IN PROGRESS..");
      Put_Line ("");

      if arg.mode = APP_MODE_PUBLISHER then

        Put_Line ("PLEASE ENTER YOUR NICKNAME");
        nickname := SU.Text_IO.Get_Line;
        Ada.Text_IO.Put_Line ("HELLO " & SU.To_String (nickname));

         pub := Chat.Publisher.Init (participant,
                                     topic,
                                     nickname);

         pub.Publish;

      elsif arg.mode = APP_MODE_SUBSCRIBER then

         Chat.Subscriber.readMessages (participant,
                                      topic
                                      );

      end if;

      if participant /= null then
         --  Perform a clean shutdown of the participant and all the contained
         --  entities
         participant.Delete_Contained_Entities;
         factory.Delete_Participant (participant);
      end if;
   exception
      when others =>
         Put_Line ("An exception happened, exiting.");
         if participant /= null then
            --  Perform a clean shutdown of the participant and all the contained
            --  entities
            participant.Delete_Contained_Entities;
            factory.Delete_Participant (participant);
         end if;

   end startApplication;

   function selectChatRoom return Chat_Room is
      room : Chat_Room := CUSTOM;
   begin
      loop
         Put_Line ("PLEASE SELECT ONE OF THE FOLLOWING CHATROOMS");
         Put_Line ("1. SPORTS");
         Put_Line ("2. BOOKS");
         Put_Line ("3. MUSIC");

         declare
            chatroom_name : constant String := Ada.Text_IO.Get_Line;
         begin
            if chatroom_name = "SPORTS" or chatroom_name = "1" then
               room := SPORTS;
            elsif  chatroom_name = "BOOKS" or chatroom_name = "2" then
               room := BOOKS;
            elsif chatroom_name = "MUSIC" or chatroom_name = "3" then
               room := MUSIC;
            end if;
            exit when chatroom_name = "SPORTS";
            exit when chatroom_name = "BOOKS";
            exit when chatroom_name = "MUSIC";
            exit when chatroom_name = "1";
            exit when chatroom_name = "2";
            exit when chatroom_name = "3";
         end;
         Ada.Text_IO.Put_Line ("ENTER A VALID CHATROOM NAME");
      end loop;

      return room;

   end selectChatRoom;

end Chat;
