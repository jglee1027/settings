set disassembly-flavor intel

define pr
  if $argc == 1
  	print (int)[$arg0 retainCount]
  end
  if $argc == 2
  	print (int)[$arg0 $arg1 retainCount]
  end
  if $argc == 3
  	print (int)[$arg0 $arg1 $arg2 retainCount]
  end
  if $argc == 4
  	print (int)[$arg0 $arg1 $arg2 $arg3 retainCount]
  end
  if $argc == 5
  	print (int)[$arg0 $arg1 $arg2 $arg3 $arg4 retainCount]
  end
end

document pr
Ask an objective-C object to print its retainCount
end

define prpv
  if $argc == 1
	set $view = $arg0
	while $view != 0
	  po $view
	  set $view = [$view superview]
	end
  end
end

document prpv
Ask an UIView object to print its parent view stack
end
