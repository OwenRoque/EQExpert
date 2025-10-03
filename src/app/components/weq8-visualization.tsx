// weq8-visualization.tsx
"use client"

import React, { useEffect, useRef } from "react"
import { WEQ8Runtime } from "weq8"
import "weq8/ui"
import { FilterType } from "@/types/weq8"

export interface EQBand {
    frequency: number
    gain: number
    q: number
    type: "Bell" | "Low S" | "High S" | "HP" | "LP"
    explanation?: string
}

interface EQVisualizationProps {
    bands: EQBand[]
}

// Map type to weq8 filter type
function getWeq8Type(type: string, q: number): FilterType {
    switch (type.toLowerCase()) {
        case "Bell": return "peaking12"
        case "Low S": return "lowshelf12"
        case "High S": return "highshelf12"
        case "HP":
            if (q == 6 || q == 12) return "highpass12"
            else if (q == 24 || q == 72) return "highpass24"
            else return "highpass12"
        case "LP":
            if (q == 6 || q == 12) return "lowpass12"
            else if (q == 24 || q == 72) return "lowpass24"
            else return "lowpass12"
        default: return "peaking12"
    }
}

export interface EQPreset {
    id: string
    title: string
    description: string
    instrument: string
    genre: string
    objective?: string
    bands: EQBand[]
    presetString: string
}

interface WEQ8UIElement extends HTMLElement {
    runtime?: WEQ8Runtime
}

// Extend the window type to include webkitAudioContext
declare global {
    interface Window {
        webkitAudioContext?: typeof AudioContext
    }
}

export default function WEQ8Visualization({ bands }: EQVisualizationProps) {
    const eqRef = useRef<WEQ8UIElement | null>(null)
    const runtimeRef = useRef<WEQ8Runtime | null>(null)

    useEffect(() => {
        const AudioCtx = window.AudioContext || window.webkitAudioContext
        const ctx = new AudioCtx()
        const runtime = new WEQ8Runtime(ctx)
        runtimeRef.current = runtime

        // Configure each band programmatically
        bands.forEach((band, i) => {
            runtime.setFilterType(i, getWeq8Type(band.type, band.q))
            runtime.toggleBypass(i, false)
            runtime.setFilterFrequency(i, band.frequency)
            runtime.setFilterQ(i, band.q)
            runtime.setFilterGain(i, band.gain)
        })

        // Connect dummy source
        const osc = ctx.createOscillator()
        osc.connect(runtime.input)
        runtime.connect(ctx.destination)
        osc.start()
        osc.stop(ctx.currentTime + 0.01)

        if (eqRef.current) {
            eqRef.current.runtime = runtime
        }

        return () => {
            runtime.disconnect(ctx.destination)
        }
    }, [bands])

    return (
        <div className="w-full rounded-lg border overflow-hidden">
            {/* @ts-expect-error: weq8-ui is a custom element not recognized by TS */}
            <weq8-ui ref={eqRef} style={{ width: "100%", height: "100%", padding: "20px", boxSizing: "border-box" }}></weq8-ui>
        </div>
    )
}
