export type FilterType =
    | "lowpass12"
    | "lowpass24"
    | "highpass12"
    | "highpass24"
    | "bandpass12"
    | "bandpass24"
    | "lowshelf12"
    | "lowshelf24"
    | "highshelf12"
    | "highshelf24"
    | "peaking12"
    | "peaking24"
    | "notch12"
    | "notch24"
    | "noop"

export interface WEQ8Filter {
    type: FilterType
    frequency: number
    Q: number
    gain: number
    bypass: boolean
}
