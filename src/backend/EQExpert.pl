% EQExpert.pl - Knowledge base for EQ suggestions with Forward Chaining

:- dynamic hecho/1.
:- dynamic justificacion/2.
:- dynamic eq_preset/4.
:- dynamic preset_info/3.
:- dynamic eq_band/6.
:- dynamic band_explanation/3.

% === Defined Presets (Base facts) ===
% eq_preset(ID, Instr, Genre, Objective)
eq_preset(preset001, bass, ambient, punchy).
eq_preset(preset002, electric_guitar, rock, clarity).

% === Titles & descriptions ===
% preset_info(ID, Title, Description)
preset_info(preset001, "Warm Ambient Bass", "A subtle punchy EQ to clean sub-bass and add presence").
preset_info(preset002, "Bright Rock Guitar", "Clarity-focused EQ to tighten lows and boost highs").

% === EQ Band per preset ===
% eq_band(PresetID, BandNumber, Type, Frequency, Gain, Q)
eq_band(preset001, 1, "Bell", 300, -3.0, 1.2).
eq_band(preset001, 2, "Bell", 1200, 2.0, 0.9).
eq_band(preset001, 3, "Low S", 8000, 1.5, 0.8).
eq_band(preset001, 5, "HP", 40, 0.0, 24.0).

eq_band(preset002, 1, "Bell", 80, -3.0, 0.6).
eq_band(preset002, 2, "Bell", 500, -2.0, 1.0).
eq_band(preset002, 3, "Bell", 2500, 3.0, 0.9).
eq_band(preset002, 4, "High S", 9000, 2.0, 0.7).

% === Explanations per band ===
% band_explanation(PresetID, BandNumber, Texto)
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

% === Reglas de inferencia para forward chaining ===
% regla(Instrumento, Objetivo, Bandas, Explicaciones)

regla(bass, muddy,
    [[1, "Bell", 250, -3.0, 1.2], [2, "Bell", 400, -2.0, 1.0]],
    ["Cuts muddiness around 250Hz", "Reduces build-up around 400Hz"]
).

regla(bass, clarity,
    [[1, "Bell", 1000, 2.5, 0.9]],
    ["Boosts mids to enhance note clarity"]
).

% === Generacion automática si no se encuentra preset ===
generar_preset(Instrumento, Objetivo, ID) :-
    regla(Instrumento, Objetivo, Bandas, Explicaciones),
    gensym("generated_", ID),
    assertz(eq_preset(ID, Instrumento, none, Objetivo)),
    assertz(preset_info(ID, "Auto-Generated Preset", "Generated via inference rules")),
    guardar_bandas(ID, Bandas, 1),
    guardar_explicaciones(ID, Explicaciones, 1),
    asserta(hecho(ID)),
    asserta(justificacion(ID, [Instrumento, Objetivo])).

guardar_bandas(_, [], _).
guardar_bandas(ID, [[N, T, F, G, Q] | Resto], N) :-
    assertz(eq_band(ID, N, T, F, G, Q)),
    N2 is N + 1,
    guardar_bandas(ID, Resto, N2).

guardar_explicaciones(_, [], _).
guardar_explicaciones(ID, [Texto | Resto], N) :-
    assertz(band_explanation(ID, N, Texto)),
    N2 is N + 1,
    guardar_explicaciones(ID, Resto, N2).

% === Punto de entrada mixto: primero intenta buscar, luego generar ===
obtener_o_generar_preset(ID, Instrumento, Genero, Objetivo) :-
    eq_preset(ID, Instrumento, Genero, Objetivo), !.
obtener_o_generar_preset(ID, Instrumento, _, Objetivo) :-
    generar_preset(Instrumento, Objetivo, ID).
