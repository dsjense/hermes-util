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
;+
; NAME:
;       GET_IDL_COMMAND
;
; PURPOSE:
;       Get an IDL command for Callable IDL from a widget interface
;       Windows interface for callable IDL
;
;
; CALLING SEQUENCE:
;
;       GET_IDL_COMMAND , Command_str
;  where 
;        Command_ str is the returned command string
;         
;  Note: Widget is active until EXIT is entered.
;  Keywords:
;    If keywords are less than the minimum they are set to the default
;    Max_stack- size of the command stack; Default = 100 , min = 10.
;               Max_stack must be set on the first call to GET_IDL_COMMAND
;    Xsize    - Size of the command window in characters; Default = 50, min = 15
;    Ysize    - Size of the history window in lines; Default = 20, min = 5
;    Mode     - If 0 one set of commands will be used.
;               If mode = 0 then widget will be killed if exit is entered.
;               If not zero, another set of commands will be used.
;               If mode ne 0 then widget will never be killed.
;    Prompt   - String to prompt for the command.
;               Once set, the value will not be changed unless the widget
;               is killed or mode changes.
;-

pro get_idl_command_init, xsize = xsize, ysize = ysize, $
   max_stack = max_stack, prompt = prompt
on_error, 1
common get_idl_command_com, gic_init, top_id, com_id, list_id, $
    prompt_id, lcselect,  max_command, max_lines, oldmode, $
    command_list, last_sent,  cur_command, command_1, command_list0, $
    command_list1, last_sent0, last_sent1, xoffset, yoffset, delta_off

    
    if n_elements (max_stack) eq 0 then max_command = 100 else begin
      if max_stack lt 10 then max_command = 100  else $
          max_command = max_stack
    endelse
    gic_init = 1
    command_list = make_array(max_command+1, /string, value = '')
    command_list0 = command_list
    command_list1 = command_list
    command_list[0] = ';  Command stack'
    last_sent = 0L
    last_sent0 = 0L
    last_sent1 = 0L
    cur_command = 0L
    command_1 = ''
    lcselect = 0 
    xoffset = 10
    yoffset = 25
    make_idl_com_wid, xsize = xsize, ysize = ysize, prompt = prompt
    return
    end

PRO command_history_event, event
    common get_idl_command_com
    lcselect = event.index
    if event.clicks eq 1 then begin 
      widget_control, com_id, set_value = command_list(lcselect)
    endif else if event.clicks eq 2 then begin
      command_1 = command_list(lcselect)
      if strlen(command_1) gt 0 then start_again
    endif
    end

pro start_again
    common get_idl_command_com
    ii = cur_command < max_command
    command_list(ii) = command_1
    if cur_command ge max_command then begin
      command_list = shift  (command_list, -1)
      command_list(max_command) = ''
      shift = lcselect
    endif else begin
      ii = ii +1
      shift = lcselect + 1
    endelse
    cur_command = cur_command +1
    widget_control, com_id, set_value = ''
    if command_1 eq command_list((shift-1)>0) then begin
      ii = min([(shift-2)>0, (ii-max_lines+1)>0])
      widget_control, list_id, set_value = command_list, $
        set_list_top = ii, set_list_select = shift
    endif else begin
      widget_control, list_id, set_value = command_list, $
        set_list_top = (ii-max_lines+1) >0, $
        set_list_select = ii
    endelse
    end

  pro get_idl_command_event, event
    common get_idl_command_com
    widget_control, event.id, get_value = command
    com = command(0)
    nelc =  n_elements(command)
    if nelc gt 1 then $
      for i = 1, nelc-1 do com= com+command(i)
    com = strtrim(com,2)
    lst = strlen(com)
    if lst gt 0 then begin
        command_1 = com
        start_again     
    endif
  end

  pro make_idl_com_wid, xsize = xsize, ysize = ysize, prompt = prompt
on_error, 1
    common get_idl_command_com
    if n_elements (xsize) eq 0 then xsize = 50 else begin
      if xsize lt 15 then xsize = 50
    endelse
    if n_elements (ysize) eq 0 then ysize = 20 else begin
      if ysize lt 5 then ysize = 20
    endelse
    max_lines = ysize
    top_id = widget_base(/column, /map)
;   top_id1 = widget_base(/column, /map, top_id, frame = 2)
;   top_id2 = widget_base(/column, /map, top_id, frame = 2)
    top_id1 = top_id
    top_id2= top_id
    label1 = widget_label(top_id1, Value = 'Command History')
    list_id = widget_list (top_id1, value = command_list, $
              ysize = max_lines)
;             ysize = max_lines , xsize = 0.5*xsize)
;             ysize = 20, event_pro='command_history_event') 
    prompt_id =  widget_label(top_id2, /dynamic_resize, $
       value= prompt)
    com_id = widget_text(top_id2, xsize = xsize, /editable, value = '') 
    widget_control, top_id, /realize, tlb_set_xoffset = xoffset, $
       tlb_set_yoffset= yoffset
    widget_control, top_id, tlb_get_offset= offset_real
    delta_off= [xoffset, yoffset] - offset_real
    widget_control, com_id , /input_focus

     
;   xmanager, 'get_idl_command', top_id, /no_block 
  end

  Pro  get_idl_command ,command_str, max_stack = max_stack, $
      xsize = xsize, ysize = ysize, prompt = prompt, mode = mode
;
on_error, 1
    common get_idl_command_com
  
    if n_elements(prompt) eq 0 then prompt = 'Enter Command Below:' else $
      if strlen(prompt) eq 0 then prompt= 'Enter Command Below:'
    if n_elements(mode) eq 0 then mode = 0
    if n_elements(gic_init) eq 0 then begin
      get_idl_command_init, prompt = prompt, $
         xsize = xsize, ysize = ysize,  max_stack = max_stack 
      oldmode = mode
    endif else begin
      widget_control, top_id, bad_id = badid
      if badid ne 0 then make_idl_com_wid, xsize = xsize, ysize = ysize, $
        prompt= prompt 
    endelse
    if mode ne oldmode then begin
      if mode eq 0 then begin
        command_list1 = command_list
        last_sent1 = last_sent
        command_list = command_list0
        last_sent = last_sent0
      endif else begin
        if oldmode eq 0 then begin
          command_list0 = command_list
          last_sent0 = last_sent
          command_list = command_list1
          last_sent = last_sent1
        endif
      endelse
      widget_control, prompt_id, set_value = prompt
      widget_control, list_id, set_value = command_list
      widget_control, com_id, set_value = ''
      widget_control, top_id, /clear_events
      oldmode = mode
      cur_command = last_sent
    endif

    while last_sent eq cur_command do begin
      event = widget_event(top_id)
      if event.id eq com_id then get_idl_command_event, event else $
        if event.id eq list_id then command_history_event, event
    endwhile
    widget_control, top_id, /clear_events
    command_str= temporary(command_1)
    last_sent = last_sent+1
    if mode ne 0  then begin
      widget_control, top_id, tlb_get_offset= offset_real
      offset_real = offset_real+delta_off
      xoffset = offset_real(0)
      yoffset = offset_real(1)
      return
    endif
    command_str= strtrim(command_str, 2)
    if strupcase(command_str) eq 'QUIT' then command_str = 'EXIT'
    if strupcase(command_str) eq 'EXIT' then begin
      widget_control, top_id, tlb_get_offset= offset_real
      offset_real = offset_real+delta_off
      xoffset = offset_real(0)
      yoffset = offset_real(1)
      widget_control, top_id, /destroy
    endif
    return
  end
