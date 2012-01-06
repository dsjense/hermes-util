; ------------------------------------------------------------------------ 
; $Id$ 
; 
; Copyright (2008) Sandia Corporation. Under the terms of
; Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
; Government retains certain rights in this software.
; 
; Hermes is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as
; published by the Free Software Foundation, either version 3 of
; the License, or (at your option) any later version.
; 
; Hermes is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Lesser General Public License for more details.
; 
; You should have received a copy of the GNU Lesser General
; Public License along with Hermes.  If not, see
; <http://www.gnu.org/licenses/>.
; 
; ------------------------------------------------------------------------ 
;
PRO winformation, message, offset = offset, xsize = xsize, ysize = ysize

;+
; This is the procedure that creates a message widget.
; format:
;       winformation, message
;-
messbase = WIDGET_BASE(TITLE = '   PFIDL Information   ', $
	/COLUMN , xsize = xsize, ysize = ysize)

; A widget for instructional text is created

wdrlabel = WIDGET_LABEL(messbase, VALUE = ' ' )
if n_elements (message) gt 0 then begin
  for i = 0, n_elements (message)-1 do $
     wdrlabel = WIDGET_LABEL(messbase, VALUE = message(i))
endif else wdrlabel = WIDGET_LABEL(messbase, VALUE = message)
wdrlabel = WIDGET_LABEL(messbase, VALUE = ' ')
;
button = WIDGET_BUTTON(messbase, VALUE = 'OK', UVALUE = 'DONE')

 if n_elements (offset) lt 2  then begin
    offset = [0, 0]
    WIDGET_CONTROL, messbase, /REALIZE
  endif else $
  WIDGET_CONTROL, messBASE, /REALIZE, tlb_set_xoffset = offset(0)> 10, $
       tlb_set_yoffset= offset(1) > 10

  event = WIDGET_EVENT (messbase)
  widgET_CONTROL, event.top, /DESTROY
end


pro HAK, dummy, message = mesg, widget_hak = widget_hak, $
              byte_return = byte_return
;+
;
; NAME:
;       HAK
; FORMAT: 
;       HAK [, CHAR ] [, MESSAGE = message] [, /WIDGET_HAK]
;           [, /BYTE_RETURN]
;  where:
;       MESSAGE:   If MESSAGE is present then:
;                  If MESSAGE is a string, print the string. 
;                  Otherwise, print, "Hit any key to continue..."
;                  If MESSAGE is omitted, nothing will be printed.
;       CHAR:      Character entered from keyboard.  Not returned if a 
;                    widget is used.
;                  Note: CHAR is a byte if it is less than 32 or
;                        greater than 126.
;       BYTE_RETURN: If set, return a byte regardless of the key struck
;       WIDGET_HAK: If set, use a widget to determine if user is ready to 
;                   continue.  No character or byte is returned.
; 
; PURPOSE:
;       HAK is a procedure that performs "Hit any key to continue".  It waits
;         for keyboard input, clears the type-ahead buffer and allows the
;         application to continue.
;       If the device is either Postscript or Encapsulated Postscript,
;         the procedure returns with no operation.
;-
;
; Check for the message keyword.
;
common get_idl_hak_command, hakflag
if n_elements(hakflag) eq 0 then $
  defsysv, "!callable_idl", exists=hakflag
if !d.name eq 'PS' then return
if !d.name eq 'PCL' then return
if hakflag eq 0 and not keyword_set(widget_hak) then begin
  if keyword_set(mesg) then begin
     sm = size(mesg)
     if sm(1) ne 7 then begin          ; Print default string
        print, 'Hit any key to continue...'
     endif else begin                  ; Print user-defined string
        print, mesg
     endelse
  endif
;
; Wait for keyboard input before continuing (returning).
;
  empty
; get the next key press
  repeat dumb = byte(get_kbrd(1)) until $ 
      ((dumb[0] gt 7B) and (dumb[0] lt 128B))
  dummy = dumb[0]
  if ~keyword_set(byte_return) && (dummy gt 31B) &&  (dummy lt 127B) $
          then  dummy = string (dummy)
; Clear the buffer
  repeat dumb = byte(get_kbrd(0)) until (dumb[0] eq 0B)
  return
endif 

; Widget HAK 
if keyword_set(mesg) then begin
   sm = size(mesg)
   if sm(1) ne 7 then begin          ; Print default string
      messag = 'Hit OK to continue...'
   endif else begin                  ; Print user-defined string
      messag = mesg
   endelse
endif else messag = 'Hit OK to continue...'
Winformation, messag, xsize = 400
dummy = ' '

end
