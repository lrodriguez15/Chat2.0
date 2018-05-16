with DDS.Builtin_String_DataReader;
with Ada.Text_IO;

package body Chat.Listener is
   use Ada.Text_IO;

   procedure On_Data_Available
     (Self       : not null access Ref;
      The_Reader : in DDS.DataReaderListener.DataReader_Access)
   is
      data_reader : constant access DDS.Builtin_String_DataReader.Ref :=
        DDS.Builtin_String_DataReader.Ref_Access (The_Reader);
      ptr_sample  : aliased DDS.String := DDS.To_DDS_String ("This is the buffer where the data will be written");
      sample_info : aliased DDS.SampleInfo;
   begin
      loop
         begin
            data_reader.Read_Next_Sample (ptr_sample, sample_info'Access);
            if sample_info.Valid_Data then
               Ada.Text_IO.Put_Line (DDS.To_Standard_String (ptr_sample));
            end if;
         exception
            when DDS.NO_DATA =>
               --  No more samples
               exit;
            when others =>
               Self.receiving := False;
               exit;
         end;
      end loop;
   end On_Data_Available;

end Chat.Listener;