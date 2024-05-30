if(.NOT. islocal) then
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: global nodes are not local nodes')
  RETURN
end if
