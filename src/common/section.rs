#[derive(Debug)]
pub enum MemRegion {
    Rom0,
    Romx,
    Vram,
    Sram,
    Wram0,
    Wramx,
    Oam,
    Hram,
}

impl MemRegion {
    pub fn min_addr(&self) -> u16 {
        match self {
            Self::Rom0 => 0x0000,
            Self::Romx => 0x4000,
            Self::Vram => 0x8000,
            Self::Sram => 0xA000,
            Self::Wram0 => 0xC000,
            Self::Wramx => 0xD000,
            Self::Oam => 0xFE00,
            Self::Hram => 0xFF80,
        }
    }
    pub fn max_addr(&self) -> u16 {
        match self {
            Self::Rom0 => 0x3FFF,
            Self::Romx => 0x7FFF,
            Self::Vram => 0x9FFF,
            Self::Sram => 0xBFFF,
            Self::Wram0 => 0xCFFF,
            Self::Wramx => 0xDFFF,
            Self::Oam => 0xFE9F,
            Self::Hram => 0xFFFE,
        }
    }
    pub fn min_bank(&self) -> u32 {
        match self {
            Self::Rom0 => 0,
            Self::Romx => 1,
            Self::Vram => 0,
            Self::Sram => 0,
            Self::Wram0 => 0,
            Self::Wramx => 1,
            Self::Oam => 0,
            Self::Hram => 0,
        }
    }
    pub fn max_bank(&self) -> u32 {
        match self {
            Self::Rom0 => 0,
            Self::Romx => u32::MAX,
            Self::Vram => 1,
            Self::Sram => u32::MAX,
            Self::Wram0 => 0,
            Self::Wramx => 7,
            Self::Oam => 0,
            Self::Hram => 0,
        }
    }
    pub fn is_banked(&self) -> bool {
        self.min_bank() != self.max_bank()
    }
}
