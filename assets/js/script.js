$(document).ready(function() {
    $("body").noiseGen({
        opacity: 1,
        width: 160,
        height: 160,
        grainDimension: 1,
        independentChannels: false,
        distribution: "bell",
        bias: 6,
        fromColor: "DBDEE3",
        toColor: "BBBEC3"
    });
    $("body > header").noiseGen({
        opacity: 1,
        width: 80,
        height: 80,
        grainDimension: 1,
        independentChannels: false,
        distribution: "bell",
        bias: 3,
        fromColor: "1C1F24",
        toColor: "272E38"
    });
});
