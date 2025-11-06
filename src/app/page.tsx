"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { ChevronRight, Music, Palette, Target, ArrowLeft, Copy, Check } from "lucide-react"
import { cn } from "@/lib/utils"
// import EQVisualization from "./components/eq-visualization"
import WEQ8Visualization, {EQBand} from "./components/weq8-visualization"
import { EQPreset } from "./components/weq8-visualization";

type Step = 1 | 2 | 3 | 4

interface Selection {
  instrument: string
  genre: string
  objective?: string
}

const instruments = [
  { id: "electric_guitar", name: "Electric Guitar", icon: "🎸" },
  { id: "bass", name: "Bass", icon: "🎸" },
  { id: "piano", name: "Piano", icon: "🎹" },
  { id: "drums", name: "Drums", icon: "🥁" },
]

const genres = [
  { id: "rock", name: "Rock", icon: "🤘" },
  { id: "edm", name: "EDM", icon: "🎧" },
  { id: "pop", name: "Pop", icon: "🎤" },
  { id: "orchestral", name: "Orchestral", icon: "🎼" },
]

const objectives = [
  { id: "punchy", name: "Make it punchy", description: "Add impact and presence" },
  { id: "muddy", name: "Too muddy", description: "Clean up low-mid frequencies" },
  { id: "clarity", name: "Add clarity", description: "Enhance definition and detail" },
  { id: "warmth", name: "Add warmth", description: "Enhance low-mid richness" },
  { id: "brightness", name: "Add brightness", description: "Enhance high frequencies" },
]

export default function EQAssistant() {
  const [currentStep, setCurrentStep] = useState<Step>(1)
  const [selection, setSelection] = useState<Selection>({
    instrument: "",
    genre: "",
    objective: undefined,
  })
  const [copiedPreset, setCopiedPreset] = useState<string | null>(null)
  const [recommendedPresets, setRecommendedPresets] = useState<EQPreset[]>([])

  const handleNext = () => {
    if (currentStep < 4) {
      const nextStep = (currentStep + 1) as Step
      setCurrentStep(nextStep)

      if (nextStep === 4) {
        fetchRecommendedPresets()
      }
    }
  }


  const handleBack = () => {
    if (currentStep > 1) {
      setCurrentStep((prev) => (prev - 1) as Step)
    }
  }

  const handleInstrumentSelect = (instrument: string) => {
    setSelection((prev) => ({ ...prev, instrument }))
  }

  const handleGenreSelect = (genre: string) => {
    setSelection((prev) => ({ ...prev, genre }))
  }

  const handleObjectiveSelect = (objective: string) => {
    setSelection((prev) => ({ ...prev, objective }))
  }

  const fetchRecommendedPresets = async () => {
    if (!selection.instrument || !selection.genre) return

    const payload = {
      instrument: selection.instrument,
      genre: selection.genre,
      objective: selection.objective || null
    }

    console.log("Payload to send:", payload)

    try {
      const controller = new AbortController()
      setTimeout(() => controller.abort(), 5000)

      const res = await fetch("/api/recommend", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "Accept": "application/json",
        },
        body: JSON.stringify(payload)
      })

      if (!res.ok) throw new Error("Failed to fetch presets")

      const data = await res.json()

      console.log("Response from backend:", data)

      setRecommendedPresets(data)
    } catch (err) {
      console.error("Error fetching presets:", err)
    }
  }

  const copyPresetString = async (presetString: string, presetId: string) => {
    try {
      await navigator.clipboard.writeText(presetString)
      setCopiedPreset(presetId)
      setTimeout(() => setCopiedPreset(null), 2000)
    } catch (err) {
      console.error("Failed to copy preset string:", err)
    }
  }

  const getStepTitle = () => {
    switch (currentStep) {
      case 1:
        return "Select Your Instrument"
      case 2:
        return "Choose Your Genre"
      case 3:
        return "What's Your Goal? (Optional)"
      case 4:
        return "Recommended EQ Settings"
      default:
        return ""
    }
  }

  const canProceed = () => {
    switch (currentStep) {
      case 1:
        return selection.instrument !== ""
      case 2:
        return selection.genre !== ""
      case 3:
        return true // Optional step
      default:
        return false
    }
  }

  return (
      <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 p-4">
        <div className="max-w-4xl mx-auto">
          {/* Header */}
          <div className="text-center mb-8 pt-8">
            <h1 className="text-4xl font-bold text-slate-900 mb-2">EQ Assistant</h1>
            <p className="text-lg text-slate-600">Get personalized equalization recommendations for your audio</p>
          </div>

          {/* Progress Indicator */}
          <div className="flex items-center justify-center mb-8">
            {[1, 2, 3, 4].map((step, index) => (
                <div key={step} className="flex items-center">
                  <div
                      className={cn(
                          "w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium transition-colors",
                          currentStep >= step ? "bg-blue-600 text-white" : "bg-slate-200 text-slate-500",
                      )}
                  >
                    {step}
                  </div>
                  {index < 3 && (
                      <div
                          className={cn(
                              "w-12 h-0.5 mx-2 transition-colors",
                              currentStep > step ? "bg-blue-600" : "bg-slate-200",
                          )}
                      />
                  )}
                </div>
            ))}
          </div>

          {/* Step Content */}
          <Card className="mb-8 shadow-lg border-0">
            <CardHeader className="text-center pb-6">
              <CardTitle className="text-2xl flex items-center justify-center gap-2">
                {currentStep === 1 && <Music className="w-6 h-6 text-blue-600" />}
                {currentStep === 2 && <Palette className="w-6 h-6 text-blue-600" />}
                {currentStep === 3 && <Target className="w-6 h-6 text-blue-600" />}
                {getStepTitle()}
              </CardTitle>
              {currentStep < 4 && (
                  <CardDescription className="text-base">
                    {currentStep === 1 && "Choose the instrument you want to equalize"}
                    {currentStep === 2 && "Select the musical genre or style"}
                    {currentStep === 3 && "Tell us what you want to achieve (you can skip this)"}
                  </CardDescription>
              )}
            </CardHeader>
            <CardContent>
              {/* Step 1: Instrument Selection */}
              {currentStep === 1 && (
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {instruments.map((instrument) => (
                        <button
                            key={instrument.id}
                            onClick={() => handleInstrumentSelect(instrument.id)}
                            className={cn(
                                "p-6 rounded-lg border-2 transition-all hover:scale-105 text-left",
                                selection.instrument === instrument.id
                                    ? "border-blue-600 bg-blue-50"
                                    : "border-slate-200 hover:border-slate-300",
                            )}
                        >
                          <div className="text-3xl mb-2">{instrument.icon}</div>
                          <div className="text-lg font-semibold text-slate-900">{instrument.name}</div>
                        </button>
                    ))}
                  </div>
              )}

              {/* Step 2: Genre Selection */}
              {currentStep === 2 && (
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {genres.map((genre) => (
                        <button
                            key={genre.id}
                            onClick={() => handleGenreSelect(genre.id)}
                            className={cn(
                                "p-6 rounded-lg border-2 transition-all hover:scale-105 text-left",
                                selection.genre === genre.id
                                    ? "border-blue-600 bg-blue-50"
                                    : "border-slate-200 hover:border-slate-300",
                            )}
                        >
                          <div className="text-3xl mb-2">{genre.icon}</div>
                          <div className="text-lg font-semibold text-slate-900">{genre.name}</div>
                        </button>
                    ))}
                  </div>
              )}

              {/* Step 3: Objective Selection */}
              {currentStep === 3 && (
                  <div className="space-y-3">
                    {objectives.map((objective) => (
                        <button
                            key={objective.id}
                            onClick={() => handleObjectiveSelect(objective.id)}
                            className={cn(
                                "w-full p-4 rounded-lg border-2 transition-all hover:scale-[1.02] text-left",
                                selection.objective === objective.id
                                    ? "border-blue-600 bg-blue-50"
                                    : "border-slate-200 hover:border-slate-300",
                            )}
                        >
                          <div className="font-semibold text-slate-900 mb-1">{objective.name}</div>
                          <div className="text-sm text-slate-600">{objective.description}</div>
                        </button>
                    ))}
                  </div>
              )}

              {/* Step 4: Results */}
              {currentStep === 4 && (
                  <div className="space-y-6">
                    <div className="text-center mb-6">
                      <div className="flex items-center justify-center gap-2 text-lg text-slate-600 mb-2">
                        <span>Recommendations for</span>
                        <Badge variant="secondary" className="text-sm">
                          {instruments.find((i) => i.id === selection.instrument)?.name}
                        </Badge>
                        <span>in</span>
                        <Badge variant="secondary" className="text-sm">
                          {genres.find((g) => g.id === selection.genre)?.name}
                        </Badge>
                        {selection.objective && (
                            <>
                              <span>to</span>
                              <Badge variant="secondary" className="text-sm">
                                {objectives.find((o) => o.id === selection.objective)?.name}
                              </Badge>
                            </>
                        )}
                      </div>
                    </div>

                    <div className="grid gap-6">
                      {Array.isArray(recommendedPresets) ? (
                        recommendedPresets.map((preset: EQPreset) => (
                          <Card key={preset.id} className="border-2 hover:border-blue-200 transition-colors">
                            <CardHeader>
                              <div className="flex items-start justify-between">
                                <div>
                                  <CardTitle className="text-xl mb-2">{preset.title}</CardTitle>
                                  <CardDescription className="text-base">{preset.description}</CardDescription>
                                </div>
                                <Button
                                    variant="outline"
                                    size="sm"
                                    onClick={() => copyPresetString(preset.presetString, preset.id)}
                                    className="ml-4 flex-shrink-0"
                                >
                                  {copiedPreset === preset.id ? (
                                      <>
                                        <Check className="w-4 h-4 mr-2" />
                                        Copied!
                                      </>
                                  ) : (
                                      <>
                                        <Copy className="w-4 h-4 mr-2" />
                                        Copy Preset
                                      </>
                                  )}
                                </Button>
                              </div>
                            </CardHeader>

                            <CardContent>
                              <div className="flex flex-col gap-6">
                                {/* WEQ8 Visualization */}
                                <div>
                                  <h4 className="font-semibold mb-3 text-slate-900">EQ Curve:</h4>
                                  <div className="w-full overflow-x-auto">
                                    <div className="min-w-[640px] h-[280px]">
                                      <WEQ8Visualization bands={preset.bands} />
                                    </div>
                                  </div>
                                </div>

                                {/* EQ Configuration Explanation */}
                                <div>
                                  <h4 className="font-semibold mb-3 text-slate-900">EQ Breakdown:</h4>
                                  <div className="space-y-2">
                                    {preset.bands.map((band: EQBand, index: number) => (
                                        <div key={index} className="text-sm">
                                          <div className="flex items-center gap-2 mb-1">
                                            <div
                                                className={cn(
                                                    "w-3 h-3 rounded-full",
                                                    band.type === "Bell" && "bg-blue-500",
                                                    band.type === "Low S" && "bg-green-500",
                                                    band.type === "High S" && "bg-yellow-400",
                                                    band.type === "HP" && "bg-red-500",
                                                    band.type === "LP" && "bg-purple-500",
                                                )}
                                            />
                                            <span className="font-medium">
                                              {band.type.charAt(0).toUpperCase() + band.type.slice(1)} at {band.frequency}Hz
                                            </span>
                                          </div>
                                          <p className="text-slate-600 ml-5">{band.explanation}</p>
                                        </div>
                                    ))}
                                  </div>
                                </div>
                              </div>
                            </CardContent>
                          </Card>
                        ))
                      ) : (
                          <p className="text-red-500">No recommendations available.</p>
                        )
                      }
                    </div>
                  </div>
              )}
            </CardContent>
          </Card>

          {/* Navigation */}
          <div className="flex justify-between">
            <Button
                variant="outline"
                onClick={handleBack}
                disabled={currentStep === 1}
                className="flex items-center gap-2"
            >
              <ArrowLeft className="w-4 h-4" />
              Back
            </Button>

            {currentStep < 4 ? (
                <Button onClick={handleNext} disabled={!canProceed()} className="flex items-center gap-2">
                  {currentStep === 3 ? "Get Recommendations" : "Next"}
                  <ChevronRight className="w-4 h-4" />
                </Button>
            ) : (
                <Button
                    variant="outline"
                    onClick={() => {
                      setCurrentStep(1)
                      setSelection({ instrument: "", genre: "", objective: undefined })
                    }}
                >
                  Start Over
                </Button>
            )}
          </div>
        </div>
      </div>
  )
}
