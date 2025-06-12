"use client"

import { useEffect, useRef } from "react"

interface EQBand {
    frequency: number
    gain: number
    q: number
    type: "bell" | "shelf" | "highpass" | "lowpass"
    explanation: string
}

interface EQVisualizationProps {
    bands: EQBand[]
}

export default function EQVisualization({ bands }: EQVisualizationProps) {
    const canvasRef = useRef<HTMLCanvasElement>(null)

    useEffect(() => {
        const canvas = canvasRef.current
        if (!canvas) return

        const ctx = canvas.getContext("2d")
        if (!ctx) return

        // Set canvas size
        const rect = canvas.getBoundingClientRect()
        canvas.width = rect.width * window.devicePixelRatio
        canvas.height = rect.height * window.devicePixelRatio
        ctx.scale(window.devicePixelRatio, window.devicePixelRatio)

        const width = rect.width
        const height = rect.height
        const padding = 20

        // Clear canvas
        ctx.clearRect(0, 0, width, height)

        // Draw grid
        ctx.strokeStyle = "#e2e8f0"
        ctx.lineWidth = 1

        // Vertical grid lines (frequency)
        const freqLines = [20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000]
        freqLines.forEach((freq) => {
            const x = padding + (Math.log10(freq / 20) / Math.log10(1000)) * (width - 2 * padding)
            if (x >= padding && x <= width - padding) {
                ctx.beginPath()
                ctx.moveTo(x, padding)
                ctx.lineTo(x, height - padding)
                ctx.stroke()
            }
        })

        // Horizontal grid lines (gain)
        for (let gain = -12; gain <= 12; gain += 3) {
            const y = height / 2 - (gain / 12) * (height / 2 - padding)
            ctx.beginPath()
            ctx.moveTo(padding, y)
            ctx.lineTo(width - padding, y)
            ctx.stroke()
        }

        // Draw frequency response curve
        ctx.strokeStyle = "#3b82f6"
        ctx.lineWidth = 3
        ctx.beginPath()

        const points = 1000
        for (let i = 0; i <= points; i++) {
            const freq = 20 * Math.pow(1000, i / points)
            const x = padding + (i / points) * (width - 2 * padding)

            let totalGain = 0
            bands.forEach((band) => {
                totalGain += calculateGainAtFrequency(freq, band)
            })

            const y = height / 2 - (totalGain / 12) * (height / 2 - padding)

            if (i === 0) {
                ctx.moveTo(x, y)
            } else {
                ctx.lineTo(x, y)
            }
        }
        ctx.stroke()

        // Draw frequency labels
        ctx.fillStyle = "#64748b"
        ctx.font = "12px system-ui"
        ctx.textAlign = "center"

        const freqLabels = [20, 100, 1000, 10000]
        freqLabels.forEach((freq) => {
            const x = padding + (Math.log10(freq / 20) / Math.log10(1000)) * (width - 2 * padding)
            if (x >= padding && x <= width - padding) {
                const label = freq >= 1000 ? `${freq / 1000}k` : `${freq}`
                ctx.fillText(label, x, height - 5)
            }
        })

        // Draw gain labels
        ctx.textAlign = "right"
        for (let gain = -12; gain <= 12; gain += 6) {
            const y = height / 2 - (gain / 12) * (height / 2 - padding)
            ctx.fillText(`${gain > 0 ? "+" : ""}${gain}dB`, padding - 5, y + 4)
        }
    }, [bands])

    const calculateGainAtFrequency = (freq: number, band: EQBand): number => {
        const { frequency, gain, q, type } = band
        const ratio = freq / frequency

        switch (type) {
            case "bell":
                const bellResponse = 1 / Math.sqrt(1 + Math.pow(q * (ratio - 1 / ratio), 2))
                return gain * bellResponse

            case "shelf":
                if (frequency < 1000) {
                    // Low shelf
                    const shelfResponse = Math.sqrt((Math.pow(10, gain / 20) - 1) * Math.pow(ratio, 2) + 1)
                    return 20 * Math.log10(shelfResponse)
                } else {
                    // High shelf
                    const shelfResponse = Math.sqrt((Math.pow(10, gain / 20) - 1) / Math.pow(ratio, 2) + 1)
                    return 20 * Math.log10(shelfResponse)
                }

            case "highpass":
                const hpResponse = Math.pow(ratio, 2) / Math.sqrt(Math.pow(ratio, 4) + Math.pow(ratio, 2) / (q * q) + 1)
                return 20 * Math.log10(hpResponse)

            case "lowpass":
                const lpResponse = 1 / Math.sqrt(Math.pow(ratio, 4) + Math.pow(ratio, 2) / (q * q) + 1)
                return 20 * Math.log10(lpResponse)

            default:
                return 0
        }
    }

    return (
        <div className="w-full h-48 bg-slate-50 rounded-lg border">
            <canvas ref={canvasRef} className="w-full h-full" style={{ width: "100%", height: "100%" }} />
        </div>
    )
}
