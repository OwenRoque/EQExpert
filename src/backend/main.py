from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from typing import Optional, List
from pyswip import Prolog
import ast

app = FastAPI()

# Configuración de CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Inicializar Prolog
prolog = Prolog()
prolog.consult("EQExpert.pl")

# Modelo para recibir JSON desde frontend
class PresetRequest(BaseModel):
    instrument: str
    genre: str
    objective: Optional[str] = None

# Static/default configuration for each band
DEFAULT_BAND_CONFIG = {
    1: {
        "Selected": "Off",
        "Active": "Off",
        "Gain": "0.0",
        "Q": "0.40",
        "Freq": "80",
        "Type": "Low S",
        "DynActive": "Off",
        "DynThreshold": "0.0",
        "DynRatio": "2.0",
        "DynSplit": "On",
        "DynAttack": "20",
        "DynRelease": "200"
    },
    2: {
        "Selected": "Off",
        "Active": "Off",
        "Gain": "0.0",
        "Q": "0.60",
        "Freq": "400",
        "Type": "Bell",
        "DynActive": "Off",
        "DynThreshold": "0.0",
        "DynRatio": "2.0",
        "DynSplit": "On",
        "DynAttack": "10",
        "DynRelease": "180"
    },
    3: {
        "Selected": "Off",
        "Active": "Off",
        "Gain": "0.0",
        "Q": "0.50",
        "Freq": "2200",
        "Type": "Bell",
        "DynActive": "Off",
        "DynThreshold": "0.0",
        "DynRatio": "2.0",
        "DynSplit": "On",
        "DynAttack": "5.0",
        "DynRelease": "160"
    },
    4: {
        "Selected": "Off",
        "Active": "Off",
        "Gain": "0.0",
        "Q": "0.60",
        "Freq": "6000",
        "Type": "High S",
        "DynActive": "Off",
        "DynThreshold": "0.0",
        "DynRatio": "2.0",
        "DynSplit": "On",
        "DynAttack": "4.0",
        "DynRelease": "23"
    },
    5: {
        "Selected": "Off",
        "Active": "Off",
        "Freq": "15",
        "Type": "HP",
        "Q": "24dB/oct"
    },
    6: {
        "Selected": "Off",
        "Active": "Off",
        "Freq": "30000",
        "Type": "LP",
        "Q": "24dB/oct"
    }
}

# Prolog stored preset to TDRNova XML
def generate_nova_preset(title, bandas):
    preset = '<TDRNova '
    if title != '':
        preset += f'_preset_name="{title}" '

    # Process each band (1-4)
    for band_num in range(1, 5):
        band = next((b for b in bandas if b['id'] == band_num), None)
        band_config = DEFAULT_BAND_CONFIG[band_num]
        # print(f"Band {band_num} config:", band_config)
        # Common attributes for all bands
        if band:
            # Band exists in input - use its values and set to active
            preset += f'bandSelected_{band_num}="Off" '
            preset += f'bandActive_{band_num}="On" '
            preset += f'bandGain_{band_num}="{band.get("gain", band_config["Gain"])}" '
            preset += f'bandQ_{band_num}="{band.get("q", band_config["Q"])}" '
            preset += f'bandFreq_{band_num}="{band.get("freq", band_config["Freq"])}" '
            preset += f'bandType_{band_num}="{band.get("type", band_config["Type"])}" '
        else:
            # Band doesn't exist - use defaults
            preset += f'bandSelected_{band_num}="{band_config["Selected"]}" '
            preset += f'bandActive_{band_num}="{band_config["Active"]}" '
            preset += f'bandGain_{band_num}="{band_config["Gain"]}" '
            preset += f'bandQ_{band_num}="{band_config["Q"]}" '
            preset += f'bandFreq_{band_num}="{band_config["Freq"]}" '
            preset += f'bandType_{band_num}="{band_config["Type"]}" '

        # Add dynamic (default) parameters for bands 1-4
        if band_num <= 4:
            preset += f'bandDynActive_{band_num}="{band_config["DynActive"]}" '
            preset += f'bandDynThreshold_{band_num}="{band_config["DynThreshold"]}" '
            preset += f'bandDynRatio_{band_num}="{band_config["DynRatio"]}" '
            preset += f'bandDynSplit_{band_num}="{band_config["DynSplit"]}" '
            preset += f'bandDynAttack_{band_num}="{band_config["DynAttack"]}" '
            preset += f'bandDynRelease_{band_num}="{band_config["DynRelease"]}" '

    # Handle HP (band 5) and LP (band 6) master parameters
    hp_band = next((b for b in bandas if b['id'] == 5), None)
    lp_band = next((b for b in bandas if b['id'] == 6), None)

    # High-pass (HP) parameters
    if hp_band:
        preset += 'hpSelected_master="Off" '
        preset += 'hpActive_master="On" '
        preset += f'hpFreq_master="{hp_band.get("freq", DEFAULT_BAND_CONFIG[5]["Freq"])}" '
        preset += f'hpType_master="{hp_band.get("type", DEFAULT_BAND_CONFIG[5]["Q"])}" '
    else:
        preset += f'hpSelected_master="{DEFAULT_BAND_CONFIG[5]["Selected"]}" '
        preset += f'hpActive_master="{DEFAULT_BAND_CONFIG[5]["Active"]}" '
        preset += f'hpFreq_master="{DEFAULT_BAND_CONFIG[5]["Freq"]}" '
        preset += f'hpType_master="{DEFAULT_BAND_CONFIG[5]["Q"]}" '

    # Low-pass (LP) parameters
    if lp_band:
        preset += 'lpSelected_master="Off" '
        preset += 'lpActive_master="On" '
        preset += f'lpFreq_master="{lp_band.get("freq", DEFAULT_BAND_CONFIG[6]["Freq"])}" '
        preset += f'lpType_master="{lp_band.get("type", DEFAULT_BAND_CONFIG[6]["Q"])}" '
    else:
        preset += f'lpSelected_master="{DEFAULT_BAND_CONFIG[6]["Selected"]}" '
        preset += f'lpActive_master="{DEFAULT_BAND_CONFIG[6]["Active"]}" '
        preset += f'lpFreq_master="{DEFAULT_BAND_CONFIG[6]["Freq"]}" '
        preset += f'lpType_master="{DEFAULT_BAND_CONFIG[6]["Q"]}" '

    # Global parameters (you can edit these if needed)
    preset += ('bandGain_wide="0.0" bandDynActive_wide="Off" bandDynThreshold_wide="0.0" bandDynRatio_wide="1.5" '
               'bandDynAttack_wide="8.0" bandDynRelease_wide="200" bypass_master="Off" delta_master="Off" '
               'dryMix_master="0.0" gain_master="0.0" eqAutoGainParam="On" qualityParam="Precise-" '
               'channelsParam="Stereo" analyzerModeParam="Analyzer: In" displayEqRangeParam="-/+ 12 dB" '
               'displayFFTRangeParam="48 dB" sidechainModeParam="Int SC" analyzerSpeedParam="Normal" solo="Off"/>')

    return preset

@app.post("/api/recommend")
async def recommend(data: PresetRequest):
    def quote(val):
        return f"'{val}'" if val else "Obj"

    instrument = quote(data.instrument)
    genre = quote(data.genre)
    objective = quote(data.objective)
    # query = f"preset_completo(ID, {instrument}, {genre}, {objective}, Titulo, Descripcion, Resultado)"

    try:
        # Obtener el ID
        # id_query = f"eq_preset(ID, {instrument}, {genre}, {objective})"
        id_query = f"obtener_o_generar_preset(ID, {instrument}, {genre}, {objective})"
        id_results = list(prolog.query(id_query))
        print(id_results)

        if not id_results:
            return JSONResponse(content={"message": "No preset found for this combination."})
        preset_id = id_results[0]["ID"]

        # Obtener bandas
        bands_query = f"preset_bands({preset_id}, Bandas)"
        bandas = list(prolog.query(bands_query))[0]["Bandas"]
        # print(bandas)

        # Explicaciones
        expl_query = f"preset_breakdown({preset_id}, Explicaciones)"
        explicaciones = list(prolog.query(expl_query))[0]["Explicaciones"]
        # print(explicaciones)

        # Titulo y descripcion
        info_query = f"preset_info({preset_id}, Titulo, Descripcion)"
        info_result = list(prolog.query(info_query))[0]
        # print(info_result)
        titulo = info_result["Titulo"]
        descripcion = info_result["Descripcion"]

        band_structs = []
        bandas_dict = []

        for i, b in enumerate(bandas):
            tipo = b[1].decode() if isinstance(b[1], bytes) else b[1]
            expl = explicaciones[i].decode() if isinstance(explicaciones[i], bytes) else explicaciones[i]
            band_structs.append({
                "frequency": float(b[2]),
                "gain": float(b[3]),
                "q": float(b[4]),
                "type": tipo.replace("_", "-"),
                "explanation": expl
            })
            bandas_dict.append({
                "id": b[0],
                "type": tipo,
                "freq": b[2],
                "gain": b[3],
                "q": b[4]
            })

        # Generar preset string
        preset_string = generate_nova_preset(
            titulo.decode() if isinstance(titulo, bytes) else titulo,
            bandas_dict
        )

        return JSONResponse(content=[{
            "id": str(preset_id),
            "title": titulo.decode() if isinstance(titulo, bytes) else titulo,
            "description": descripcion.decode() if isinstance(descripcion, bytes) else descripcion,
            "bands": band_structs,
            "presetString": preset_string
        }])

    except Exception as e:
        return JSONResponse(content={"error": str(e)}, status_code=500)

@app.get("/healthz")
def healthz():
    return {"status": "ok"}
