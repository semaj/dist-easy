ports = [8091, 8092, 8093, 8094]
ip = "127.0.0.01"
threads = []

ports.each do |p|
  puts "starting #{p}"
  others = ports.dup
  others.delete(p)
  matched = others.map { |x| "#{ip}:#{x}" }.join(' ')
  threads << Thread.new { system("stack exec --allow-different-user dist-test -- #{ip}:#{p} #{matched}") }
end

threads.map(&:join)
