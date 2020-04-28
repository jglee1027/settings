function os.capture(cmd, raw)
   local f = assert(io.popen(cmd, 'r'))
   local s = assert(f:read('*a'))

   f:close()

   if raw then
      return s
   end

   s = string.gsub(s, '^%s+', '')
   s = string.gsub(s, '%s+$', '')
   s = string.gsub(s, '[\n\r]+', ' ')

   return s
end

function conky_network(ifname)
   local res = ""
   local stat = os.capture("ip link | grep 'state UP' | grep \"" .. ifname .. ":\" | cut -d ' ' -f 2 | sed s/:// ")

   if stat == "" then
      -- only display ifnames that it's state is up
      return
   end

   local ssid = os.capture("/sbin/iwgetid --raw " .. ifname)

   res = res.format("${color #0077ff}%-10s IP: ${color}${addrs ", ifname) .. ifname .. "} " .. ssid .. "\n" ..
      "${color #0077ff} Net Down:$color ${downspeed " .. ifname .. "}k/s${alignr}${color #0077ff}Net Up:$color ${upspeed " .. ifname .. "}k/s\n" ..
      "${color #0077ff} ${downspeedgraph " .. ifname .. " 24,155 104E8B 0077ff} $alignr${color #0077ff}${upspeedgraph " .. ifname .. " 24,155 104E8B 0077ff}"
   return res
end

function conky_network_all()
   local res = ""
   local ifnames = os.capture("ls /sys/class/net")
   for ifname in string.gmatch(ifnames, "%S+") do
      local stat = os.capture("cat /sys/class/net/" .. ifname .. "/operstate")
      if stat == "up" then
         local str = conky_network(ifname)
         if str ~= nil then
            if res ~= "" then
               res = res .. "\n"
            end
            res = res .. str
         end
      end
   end
   return res
end
