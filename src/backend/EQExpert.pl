% EQExpert.pl - Knowledge base for EQ suggestions with Forward Chaining
:- dynamic hecho/1.
:- dynamic justificacion/2.
:- dynamic eq_preset/4.
:- dynamic preset_info/3.
:- dynamic eq_band/6.
:- dynamic band_explanation/3.

% === Defined Presets (Base facts) ===
% (mantengo los preset originales como ejemplos)
eq_preset(preset001, bass, ambient, punchy).
eq_preset(preset002, electric_guitar, rock, clarity).

% === Titles & descriptions ===
preset_info(preset001, "Warm Ambient Bass", "A subtle punchy EQ to clean sub-bass and add presence").
preset_info(preset002, "Bright Rock Guitar", "Clarity-focused EQ to tighten lows and boost highs").

% === EQ Band per preset (ejemplos existentes) ===
eq_band(preset001, 1, "Bell", 300, -3.0, 1.2).
eq_band(preset001, 2, "Bell", 1200, 2.0, 0.9).
eq_band(preset001, 3, "Low S", 8000, 1.5, 0.8).
eq_band(preset001, 5, "HP", 40, 0.0, 24.0).

eq_band(preset002, 1, "Bell", 80, -3.0, 0.6).
eq_band(preset002, 2, "Bell", 500, -2.0, 1.0).
eq_band(preset002, 3, "Bell", 2500, 3.0, 0.9).
eq_band(preset002, 4, "High S", 9000, 2.0, 0.7).

% === Explanations per band (ejemplos existentes) ===
band_explanation(preset001, 1, "Reduces boxiness for a tighter sound").
band_explanation(preset001, 2, "Adds mid presence to cut through the mix").
band_explanation(preset001, 3, "Adds air and brightness to the top end").
band_explanation(preset001, 5, "Removes sub-rumble to clean the low end").

band_explanation(preset002, 1, "Cleans up unnecessary low-end").
band_explanation(preset002, 2, "Removes muddiness around 500Hz").
band_explanation(preset002, 3, "Boosts clarity and presence").
band_explanation(preset002, 4, "Adds sparkle in the highs").

% === Wrapper rules ===
preset_bands(ID, Bands) :-
    findall([N, T, F, G, Q], eq_band(ID, N, T, F, G, Q), Bands).

preset_breakdown(ID, Explanations) :-
    findall(Text, band_explanation(ID, _, Text), Explanations).

full_preset(ID, Instrument, Genre, Objective, Title, Description, Result) :-
    eq_preset(ID, Instrument, Genre, Objective),
    preset_bands(ID, Bandas),
    preset_breakdown(ID, Explanations),
    preset_info(ID, Title, Description),
    Result = [bandas:Bandas, explicaciones:Explanations].

% === Forward chaining rules ===

% Modificado para manejar multiples coincidencias
encadenar(Instrument, Genre, Objective) :-
    retractall(hecho(_)),
    retractall(justificacion(_, _)),
    findall(ID, eq_preset(ID, Instrument, Genre, Objective), IDs),
    guardar_hechos(IDs, Instrument, Genre, Objective).

guardar_hechos([], _, _, _).
guardar_hechos([ID | Rest], Instrument, Genre, Objective) :-
    asserta(hecho(ID)),
    asserta(justificacion(ID, [Instrument, Genre, Objective])),
    guardar_hechos(Rest, Instrument, Genre, Objective).

% Consulta explicativa
sugerencia(ID, Justificacion) :-
    hecho(ID),
    justificacion(ID, Justificacion).

% === Reglas de inferencia base ===
% regla_base(Instrumento, Objetivo, Bandas, Explicaciones).
% Bandas: lista de [Pos, Tipo, Freq, Gain, Q]
% Explicaciones: lista de textos

% ---- BASS rules
regla_base(bass, punchy,
    [[1,"HP",40,0.0,24.0],[2,"Bell",120,3.0,0.9],[3,"Bell",800,1.5,0.8]],
    ["High-pass to tighten the low end","Boost low-mid for punch","Small presence boost"]
).

regla_base(bass, muddy,
    [[1,"HP",35,0.0,24.0],[2,"Bell",250,-3.0,1.2],[3,"Bell",600,-2.0,1.0]],
    ["Remove sub-rumble","Cut muddiness around 250Hz","Reduce build-up in low mids"]
).

regla_base(bass, clarity,
    [[1,"HP",40,0.0,24.0],[2,"Bell",800,2.5,0.9],[3,"Bell",2500,1.5,0.8]],
    ["Clean sub-low","Boost mids to clarify notes","Add presence in upper mids"]
).

regla_base(bass, warmth,
    [[1,"HP",30,0.0,24.0],[2,"Bell",100,2.0,0.9],[3,"Bell",400,1.0,1.0]],
    ["Gentle low boost","Add warmth around 100Hz","Slight body in low mids"]
).

regla_base(bass, brightness,
    [[1,"HP",40,0.0,24.0],[2,"Bell",800,1.0,0.9],[3,"High S",8000,2.0,0.7]],
    ["Keep low end clean","Add upper-mid clarity","Air and brightness"]
).

% ---- ELECTRIC GUITAR rules
regla_base(electric_guitar, punchy,
    [[1,"Bell",80,-2.0,0.7],[2,"Bell",250,2.0,1.0],[3,"Bell",1200,2.5,0.9],[4,"High S",9000,1.0,0.7]],
    ["Remove excess low-end","Add low-mid punch","Presence for articulation","Sparkle"]
).

regla_base(electric_guitar, muddy,
    [[1,"Bell",100,-3.0,0.8],[2,"Bell",500,-2.5,1.0],[3,"Bell",2500,2.0,0.9]],
    ["Clean low-mids","Cut mud around 500Hz","Bring clarity in upper mids"]
).

regla_base(electric_guitar, clarity,
    [[1,"Bell",120, -1.0,0.8],[2,"Bell",800,2.5,0.9],[3,"Bell",3000,3.0,0.8]],
    ["Tighten low string content","Boost mids for note definition","Boost upper mids for clarity"]
).

regla_base(electric_guitar, warmth,
    [[1,"Bell",80,2.0,0.8],[2,"Bell",300,1.0,1.0],[3,"Bell",2000,-1.0,1.0]],
    ["Add body to lower strings","Gentle mid warmth","Slight reduction in harsh upper-mid"]
).

regla_base(electric_guitar, brightness,
    [[1,"Bell",120,-1.0,0.8],[2,"Bell",250,0.0,1.0],[3,"High S",10000,3.0,0.6]],
    ["Control low-mids","Neutral mid","Big air and sparkle"]
).

% ---- PIANO rules
regla_base(piano, punchy,
    [[1,"HP",30,0.0,24.0],[2,"Bell",120,2.0,0.9],[3,"Bell",1000,1.5,0.9]],
    ["Remove extreme sub","Low-mid boost for punch","Presence on keys"]
).

regla_base(piano, muddy,
    [[1,"HP",28,0.0,24.0],[2,"Bell",300,-3.0,1.2],[3,"Bell",700,-1.5,1.0]],
    ["Clean sub-lows","Cut muddiness in lower-mid","Reduce build-up"]
).

regla_base(piano, clarity,
    [[1,"Bell",400,2.5,0.9],[2,"Bell",2000,2.0,0.8],[3,"High S",10000,1.5,0.7]],
    ["Enhance note definition","Upper-mid clarity","Add air"]
).

regla_base(piano, warmth,
    [[1,"Bell",100,2.0,0.9],[2,"Bell",300,1.0,1.0],[3,"Bell",800,-0.5,1.0]],
    ["Warmth on lower registers","Body in low-mid","Tame some upper-mid harshness"]
).

regla_base(piano, brightness,
    [[1,"Bell",300,0.5,1.0],[2,"Bell",2000,1.5,0.8],[3,"High S",12000,2.5,0.6]],
    ["Gentle boost in upper-mid","Clarity","Air"]
).

% ---- DRUMS rules
regla_base(drums, punchy,
    [[1,"Bell",60,3.0,0.8],[2,"Bell",250,2.0,1.0],[3,"Bell",4000,2.0,0.9]],
    ["Boomy low tuned for punch","Attack in low-mid","Snap on top end"]
).

regla_base(drums, muddy,
    [[1,"HP",30,0.0,24.0],[2,"Bell",200,-3.0,1.2],[3,"Bell",800,-1.5,1.0]],
    ["Cut sub rumble","Remove boxiness","Reduce boxy build-up"]
).

regla_base(drums, clarity,
    [[1,"Bell",50,1.5,0.9],[2,"Bell",300,2.0,1.0],[3,"Bell",3000,2.5,0.8]],
    ["Round low for punch","Clear mid transient","Bring cymbal presence"]
).

regla_base(drums, warmth,
    [[1,"Bell",60,2.0,0.9],[2,"Bell",200,1.0,1.0],[3,"Bell",800,0.5,1.0]],
    ["Add low warmth","Gentle low-mid","Soften hardness"]
).

regla_base(drums, brightness,
    [[1,"Bell",60,0.0,0.9],[2,"Bell",800,0.5,1.0],[3,"High S",10000,3.0,0.6]],
    ["Keep low balanced","Slight upper-mid","Cymbal sparkle"]
).

% === Genre tweak mechanism ===
% Small gain offsets so presets del mismo objetivo varían según género.
genre_gain_offset(rock, 0.5).
genre_gain_offset(edm, 0.8).
genre_gain_offset(pop, 0.3).
genre_gain_offset(orchestral, -0.2).
genre_gain_offset(ambient, 0.1).
% default
genre_gain_offset(_, 0.0).

tweak_bands_for_genre([], _, []).
tweak_bands_for_genre([[Pos, Type, Freq, Gain, Q] | Rest], Genre, [[Pos, Type, Freq, NewGain, Q] | RestT]) :-
    % we will match Genre with lowercase if needed
    genre_gain_offset(GenreLower, Offset),
    (   atom(Genre) -> atom_string(Genre, Gs), string_lower(Gs, GLower)
    ;   GLower = Genre
    ),
    % map string to atom used in genre_gain_offset
    (   (GLower = "rock" -> GenreLower = rock);
        (GLower = "edm" -> GenreLower = edm);
        (GLower = "pop" -> GenreLower = pop);
        (GLower = "orchestral" -> GenreLower = orchestral);
        (GLower = "ambient" -> GenreLower = ambient);
        (GenreLower = other)
    ),
    genre_gain_offset(GenreLower, Offset),
    NewGain is Gain + Offset,
    tweak_bands_for_genre(Rest, Genre, RestT).

% === Guardado correcto de bandas y explicaciones (fix) ===
guardar_bandas(_, [], _).
guardar_bandas(ID, [[_Pos, Tipo, Freq, Gain, Q] | Resto], Index) :-
    assertz(eq_band(ID, Index, Tipo, Freq, Gain, Q)),
    Index2 is Index + 1,
    guardar_bandas(ID, Resto, Index2).

guardar_explicaciones(_, [], _).
guardar_explicaciones(ID, [Texto | Resto], Index) :-
    assertz(band_explanation(ID, Index, Texto)),
    Index2 is Index + 1,
    guardar_explicaciones(ID, Resto, Index2).

% === Generacion automática si no se encuentra preset ===
% ahora recibe Genero para aplicar tweak
generar_preset(Instrumento, Genero, Objetivo, ID) :-
    regla_base(Instrumento, Objetivo, Bandas, Explicaciones),
    tweak_bands_for_genre(Bandas, Genero, BandasTweaked),
    gensym("generated_", ID),
    assertz(eq_preset(ID, Instrumento, Genero, Objetivo)),
    assertz(preset_info(ID, "Auto-Generated Preset", "Generated via inference rules")),
    guardar_bandas(ID, BandasTweaked, 1),
    guardar_explicaciones(ID, Explicaciones, 1),
    asserta(hecho(ID)),
    asserta(justificacion(ID, [Instrumento, Genero, Objetivo])).

% === Punto de entrada mixto: primero intenta buscar, luego generar ===
% obtener_o_generar_preset(ID, Instrumento, Genero, Objetivo)
obtener_o_generar_preset(ID, Instrumento, Genero, Objetivo) :-
    eq_preset(ID, Instrumento, Genero, Objetivo), !.
obtener_o_generar_preset(ID, Instrumento, Genero, Objetivo) :-
    generar_preset(Instrumento, Genero, Objetivo, ID).

% Helper: listar todas las combinaciones posibles registradas
% useful for testing
lista_presets_disponibles(Combos) :-
    findall([ID,Instr,Gen,Obj], eq_preset(ID,Instr,Gen,Obj), Combos).
