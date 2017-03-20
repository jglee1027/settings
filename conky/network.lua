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

function conky_network(iface_type, iface)
   local res = ""
   local iface = os.capture("ip link | grep 'state UP' | grep " .. iface .. "| cut -d ' ' -f 2 | sed s/:// ")

   if iface  == "" then
      res = res.format("${color #0077ff}%-8s IP: ${color}", iface_type) .. "DOWN"
      return res
   end

   local ssid = os.capture("/sbin/iwgetid --raw " .. iface)

   res = res.format("${color #0077ff}%-8s IP: ${color}${addrs ", iface_type) .. iface .. "} " .. ssid .. "\n" ..
      "${color #0077ff} Net Down:$color ${downspeed " .. iface .. "}k/s${alignr}${color #0077ff}Net Up:$color ${upspeed " .. iface .. "}k/s\n" ..
      "${color #0077ff} ${downspeedgraph " .. iface .. " 24,155 104E8B 0077ff} $alignr${color #0077ff}${upspeedgraph " .. iface .. " 24,155 104E8B 0077ff}"
   return res
end
