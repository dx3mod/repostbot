let last_date_vk_record (record : Vkashka.Wall.Record.t) =
  Option.value record.edited ~default:record.date
