import Link from "next/link";
import Image from "next/image";
import { courses } from "@/lib/courses";
import { ThemeToggle } from "@/components/theme-toggle";
import logo from "../logo.png";

const courseLogos: Record<string, React.ReactNode> = {
  go: (
    <svg viewBox="0 0 24 24" className="w-8 h-8" fill="#00ADD8">
      <path d="M1.811 10.231c-.047 0-.058-.023-.035-.059l.246-.315c.023-.035.081-.058.128-.058h4.172c.046 0 .058.035.035.07l-.199.303c-.023.036-.082.07-.117.07zM.047 11.306c-.047 0-.059-.023-.035-.058l.245-.316c.023-.035.082-.058.129-.058h5.328c.047 0 .07.035.058.07l-.093.28c-.012.047-.058.07-.105.07zm2.828 1.075c-.047 0-.059-.035-.035-.07l.163-.292c.023-.035.07-.07.117-.07h2.337c.047 0 .07.035.07.082l-.023.28c0 .047-.047.082-.082.082zm12.129-2.36c-.736.187-1.239.327-1.963.514-.176.046-.187.058-.34-.117-.174-.199-.303-.327-.548-.444-.737-.362-1.45-.257-2.115.175-.795.514-1.204 1.274-1.192 2.22.011.935.654 1.706 1.577 1.835.795.105 1.46-.175 1.987-.77.105-.13.198-.27.315-.434H10.47c-.245 0-.304-.152-.222-.35.152-.362.432-.97.596-1.274a.315.315 0 01.292-.187h4.253c-.023.316-.023.631-.07.947a4.983 4.983 0 01-.958 2.29c-.841 1.11-1.94 1.8-3.33 1.986-1.145.152-2.209-.07-3.143-.77-.865-.655-1.356-1.52-1.484-2.595-.152-1.274.222-2.419.993-3.424.83-1.086 1.928-1.776 3.272-2.02 1.098-.2 2.15-.07 3.096.571.62.41 1.063.97 1.356 1.648.07.105.023.164-.117.2m3.868 6.461c-1.064-.024-2.034-.328-2.852-1.029a3.665 3.665 0 01-1.262-2.255c-.21-1.32.152-2.489.947-3.529.853-1.122 1.881-1.706 3.272-1.95 1.192-.21 2.314-.095 3.33.595.923.63 1.496 1.484 1.648 2.605.198 1.578-.257 2.863-1.344 3.962-.771.783-1.718 1.273-2.805 1.495-.315.06-.63.07-.934.106zm2.78-4.72c-.011-.153-.011-.27-.034-.387-.21-1.157-1.274-1.81-2.384-1.554-1.087.245-1.788.935-2.045 2.033-.21.912.234 1.835 1.075 2.21.643.28 1.285.244 1.905-.07.923-.48 1.425-1.228 1.484-2.233z"/>
    </svg>
  ),
  zig: (
    <svg viewBox="0 0 24 24" className="w-8 h-8" fill="#F7A41D">
      <path d="m23.53 1.02-7.686 3.45h-7.06l-2.98 3.452h7.173L.47 22.98l7.681-3.607h7.065v-.002l2.978-3.45-7.148-.001 12.482-14.9zM0 4.47v14.901h1.883l2.98-3.45H3.451v-8h.942l2.824-3.45H0zm22.117 0-2.98 3.608h1.412v7.844h-.942l-2.98 3.45H24V4.47h-1.883z"/>
    </svg>
  ),
  postgresql: (
    <svg viewBox="0 0 24 24" className="w-8 h-8" fill="#4169E1">
      <path d="M23.5594 14.7228a.5269.5269 0 0 0-.0563-.1191c-.139-.2632-.4768-.3418-1.0074-.2321-1.6533.3411-2.2935.1312-2.5256-.0191 1.342-2.0482 2.445-4.522 3.0411-6.8297.2714-1.0507.7982-3.5237.1222-4.7316a1.5641 1.5641 0 0 0-.1509-.235C21.6931.9086 19.8007.0248 17.5099.0005c-1.4947-.0158-2.7705.3461-3.1161.4794a9.449 9.449 0 0 0-.5159-.0816 8.044 8.044 0 0 0-1.3114-.1278c-1.1822-.0184-2.2038.2642-3.0498.8406-.8573-.3211-4.7888-1.645-7.2219.0788C.9359 2.1526.3086 3.8733.4302 6.3043c.0409.818.5069 3.334 1.2423 5.7436.4598 1.5065.9387 2.7019 1.4334 3.582.553.9942 1.1259 1.5933 1.7143 1.7895.4474.1491 1.1327.1441 1.8581-.7279.8012-.9635 1.5903-1.8258 1.9446-2.2069.4351.2355.9064.3625 1.39.3772a.0569.0569 0 0 0 .0004.0041 11.0312 11.0312 0 0 0-.2472.3054c-.3389.4302-.4094.5197-1.5002.7443-.3102.064-1.1344.2339-1.1464.8115-.0025.1224.0329.2309.0919.3268.2269.4231.9216.6097 1.015.6331 1.3345.3335 2.5044.092 3.3714-.6787-.017 2.231.0775 4.4174.3454 5.0874.2212.5529.7618 1.9045 2.4692 1.9043.2505 0 .5263-.0291.8296-.0941 1.7819-.3821 2.5557-1.1696 2.855-2.9059.1503-.8707.4016-2.8753.5388-4.1012.0169-.0703.0357-.1207.057-.1362.0007-.0005.0697-.0471.4272.0307a.3673.3673 0 0 0 .0443.0068l.2539.0223.0149.001c.8468.0384 1.9114-.1426 2.5312-.4308.6438-.2988 1.8057-1.0323 1.5951-1.6698zM2.371 11.8765c-.7435-2.4358-1.1779-4.8851-1.2123-5.5719-.1086-2.1714.4171-3.6829 1.5623-4.4927 1.8367-1.2986 4.8398-.5408 6.108-.13-.0032.0032-.0066.0061-.0098.0094-2.0238 2.044-1.9758 5.536-1.9708 5.7495-.0002.0823.0066.1989.0162.3593.0348.5873.0996 1.6804-.0735 2.9184-.1609 1.1504.1937 2.2764.9728 3.0892.0806.0841.1648.1631.2518.2374-.3468.3714-1.1004 1.1926-1.9025 2.1576-.5677.6825-.9597.5517-1.0886.5087-.3919-.1307-.813-.5871-1.2381-1.3223-.4796-.839-.9635-2.0317-1.4155-3.5126zm6.0072 5.0871c-.1711-.0428-.3271-.1132-.4322-.1772.0889-.0394.2374-.0902.4833-.1409 1.2833-.2641 1.4815-.4506 1.9143-1.0002.0992-.126.2116-.2687.3673-.4426a.3549.3549 0 0 0 .0737-.1298c.1708-.1513.2724-.1099.4369-.0417.156.0646.3078.26.3695.4752.0291.1016.0619.2945-.0452.4444-.9043 1.2658-2.2216 1.2494-3.1676 1.0128zm2.094-3.988-.0525.141c-.133.3566-.2567.6881-.3334 1.003-.6674-.0021-1.3168-.2872-1.8105-.8024-.6279-.6551-.9131-1.5664-.7825-2.5004.1828-1.3079.1153-2.4468.079-3.0586-.005-.0857-.0095-.1607-.0122-.2199.2957-.2621 1.6659-.9962 2.6429-.7724.4459.1022.7176.4057.8305.928.5846 2.7038.0774 3.8307-.3302 4.7363-.084.1866-.1633.3629-.2311.5454zm7.3637 4.5725c-.0169.1768-.0358.376-.0618.5959l-.146.4383a.3547.3547 0 0 0-.0182.1077c-.0059.4747-.054.6489-.115.8693-.0634.2292-.1353.4891-.1794 1.0575-.11 1.4143-.8782 2.2267-2.4172 2.5565-1.5155.3251-1.7843-.4968-2.0212-1.2217a6.5824 6.5824 0 0 0-.0769-.2266c-.2154-.5858-.1911-1.4119-.1574-2.5551.0165-.5612-.0249-1.9013-.3302-2.6462.0044-.2932.0106-.5909.019-.8918a.3529.3529 0 0 0-.0153-.1126 1.4927 1.4927 0 0 0-.0439-.208c-.1226-.4283-.4213-.7866-.7797-.9351-.1424-.059-.4038-.1672-.7178-.0869.067-.276.1831-.5875.309-.9249l.0529-.142c.0595-.16.134-.3257.213-.5012.4265-.9476 1.0106-2.2453.3766-5.1772-.2374-1.0981-1.0304-1.6343-2.2324-1.5098-.7207.0746-1.3799.3654-1.7088.5321a5.6716 5.6716 0 0 0-.1958.1041c.0918-1.1064.4386-3.1741 1.7357-4.4823a4.0306 4.0306 0 0 1 .3033-.276.3532.3532 0 0 0 .1447-.0644c.7524-.5706 1.6945-.8506 2.802-.8325.4091.0067.8017.0339 1.1742.081 1.939.3544 3.2439 1.4468 4.0359 2.3827.8143.9623 1.2552 1.9315 1.4312 2.4543-1.3232-.1346-2.2234.1268-2.6797.779-.9926 1.4189.543 4.1729 1.2811 5.4964.1353.2426.2522.4522.2889.5413.2403.5825.5515.9713.7787 1.2552.0696.087.1372.1714.1885.245-.4008.1155-1.1208.3825-1.0552 1.717-.0123.1563-.0423.4469-.0834.8148-.0461.2077-.0702.4603-.0994.7662zm.8905-1.6211c-.0405-.8316.2691-.9185.5967-1.0105a2.8566 2.8566 0 0 0 .135-.0406 1.202 1.202 0 0 0 .1342.103c.5703.3765 1.5823.4213 3.0068.1344-.2016.1769-.5189.3994-.9533.6011-.4098.1903-1.0957.333-1.7473.3636-.7197.0336-1.0859-.0807-1.1721-.151zm.5695-9.2712c-.0059.3508-.0542.6692-.1054 1.0017-.055.3576-.112.7274-.1264 1.1762-.0142.4368.0404.8909.0932 1.3301.1066.887.216 1.8003-.2075 2.7014a3.5272 3.5272 0 0 1-.1876-.3856c-.0527-.1276-.1669-.3326-.3251-.6162-.6156-1.1041-2.0574-3.6896-1.3193-4.7446.3795-.5427 1.3408-.5661 2.1781-.463zm.2284 7.0137a12.3762 12.3762 0 0 0-.0853-.1074l-.0355-.0444c.7262-1.1995.5842-2.3862.4578-3.4385-.0519-.4318-.1009-.8396-.0885-1.2226.0129-.4061.0666-.7543.1185-1.0911.0639-.415.1288-.8443.1109-1.3505.0134-.0531.0188-.1158.0118-.1902-.0457-.4855-.5999-1.938-1.7294-3.253-.6076-.7073-1.4896-1.4972-2.6889-2.0395.5251-.1066 1.2328-.2035 2.0244-.1859 2.0515.0456 3.6746.8135 4.8242 2.2824a.908.908 0 0 1 .0667.1002c.7231 1.3556-.2762 6.2751-2.9867 10.5405zm-8.8166-6.1162c-.025.1794-.3089.4225-.6211.4225a.5821.5821 0 0 1-.0809-.0056c-.1873-.026-.3765-.144-.5059-.3156-.0458-.0605-.1203-.178-.1055-.2844.0055-.0401.0261-.0985.0925-.1488.1182-.0894.3518-.1226.6096-.0867.3163.0441.6426.1938.6113.4186zm7.9305-.4114c.0111.0792-.049.201-.1531.3102-.0683.0717-.212.1961-.4079.2232a.5456.5456 0 0 1-.075.0052c-.2935 0-.5414-.2344-.5607-.3717-.024-.1765.2641-.3106.5611-.352.297-.0414.6111.0088.6356.1851z"/>
    </svg>
  ),
  arm64: (
    <svg viewBox="0 0 24 24" className="w-8 h-8" fill="#0091BD">
      <path d="M5.419 8.534h1.614v6.911H5.419v-.72c-.71.822-1.573.933-2.07.933C1.218 15.658 0 13.882 0 11.985c0-2.253 1.542-3.633 3.37-3.633.507 0 1.4.132 2.049.984zm-3.765 3.491c0 1.198.751 2.202 1.918 2.202 1.015 0 1.959-.74 1.959-2.181 0-1.512-.934-2.233-1.959-2.233-1.167-.01-1.918.974-1.918 2.212zm7.297-3.49h1.613v.618a3 3 0 0 1 .67-.578c.314-.183.619-.233.984-.233.396 0 .822.06 1.269.324l-.66 1.462a1.432 1.432 0 0 0-.822-.244c-.345 0-.69.05-1.005.376-.446.477-.446 1.136-.446 1.593v3.582H8.94zm5.56 0h1.614v.639c.538-.66 1.177-.822 1.705-.822.72 0 1.4.345 1.786 1.015.579-.822 1.441-1.015 2.05-1.015.842 0 1.573.396 1.969 1.086.132.233.365.74.365 1.745v4.272h-1.614V11.65c0-.771-.08-1.086-.152-1.228-.101-.264-.345-.609-.923-.609-.396 0-.741.213-.954.508-.284.395-.315.984-.315 1.572v3.562H18.43V11.65c0-.771-.081-1.086-.152-1.228-.102-.264-.345-.609-.924-.609-.396 0-.74.213-.954.508-.284.395-.314.984-.314 1.572v3.562h-1.573z"/>
    </svg>
  ),
  c: (
    <svg viewBox="0 0 24 24" className="w-8 h-8" fill="#A8B9CC">
      <path d="M16.5921 9.1962s-.354-3.298-3.627-3.39c-3.2741-.09-4.9552 2.474-4.9552 6.14 0 3.6651 1.858 6.5972 5.0451 6.5972 3.184 0 3.5381-3.665 3.5381-3.665l6.1041.365s.36 3.31-2.196 5.836c-2.552 2.5241-5.6901 2.9371-7.8762 2.9201-2.19-.017-5.2261.034-8.1602-2.97-2.938-3.0101-3.436-5.9302-3.436-8.8002 0-2.8701.556-6.6702 4.047-9.5502C7.444.72 9.849 0 12.254 0c10.0422 0 10.7172 9.2602 10.7172 9.2602z"/>
    </svg>
  ),
  r: (
    <svg viewBox="0 0 724 561" className="w-8 h-8">
      <defs>
        <linearGradient id="rGrad1" x1="0" x2="1" y1="0" y2="1" gradientUnits="objectBoundingBox">
          <stop offset="0" stopColor="rgb(203,206,208)"/>
          <stop offset="1" stopColor="rgb(132,131,139)"/>
        </linearGradient>
        <linearGradient id="rGrad2" x1="0" x2="1" y1="0" y2="1" gradientUnits="objectBoundingBox">
          <stop offset="0" stopColor="rgb(39,109,195)"/>
          <stop offset="1" stopColor="rgb(22,92,170)"/>
        </linearGradient>
      </defs>
      <path d="M361.453,485.937 C162.329,485.937 0.906,377.828 0.906,244.469 C0.906,111.109 162.329,3.000 361.453,3.000 C560.578,3.000 722.000,111.109 722.000,244.469 C722.000,377.828 560.578,485.937 361.453,485.937 ZM416.641,97.406 C265.289,97.406 142.594,171.314 142.594,262.484 C142.594,353.654 265.289,427.562 416.641,427.562 C567.992,427.562 679.687,377.033 679.687,262.484 C679.687,147.971 567.992,97.406 416.641,97.406 Z" fill="url(#rGrad1)" fillRule="evenodd"/>
      <path d="M550.000,377.000 C550.000,377.000 571.822,383.585 584.500,390.000 C588.899,392.226 596.510,396.668 602.000,402.500 C607.378,408.212 610.000,414.000 610.000,414.000 L696.000,559.000 L557.000,559.062 L492.000,437.000 C492.000,437.000 478.690,414.131 470.500,407.500 C463.668,401.969 460.755,400.000 454.000,400.000 C449.298,400.000 420.974,400.000 420.974,400.000 L421.000,558.974 L298.000,559.026 L298.000,152.938 L545.000,152.938 C545.000,152.938 657.500,154.967 657.500,262.000 C657.500,369.033 550.000,377.000 550.000,377.000 ZM496.500,241.024 L422.037,240.976 L422.000,310.026 L496.500,310.002 C496.500,310.002 531.000,309.895 531.000,274.877 C531.000,239.155 496.500,241.024 496.500,241.024 Z" fill="url(#rGrad2)" fillRule="evenodd"/>
    </svg>
  ),
  gleam: (
    <svg viewBox="0 0 2105 2016" className="w-8 h-8" fill="none">
      <path d="M842.026 129.177C870.114 49.5833 974.531 31.1719 1028.15 96.3575L1309.17 438.02C1343.94 480.296 1395.4 505.347 1450.14 506.68L1892.78 517.45C1977.29 519.505 2026.92 612.947 1981.45 683.983L1742.87 1056.65C1728.28 1079.43 1718.77 1105.09 1715 1131.88C1711.22 1158.66 1713.27 1185.95 1721 1211.87L1847.34 1635.7C1871.42 1716.45 1797.88 1792.71 1716.03 1771.44L1287.55 1660.11C1261.36 1653.3 1234.01 1652.21 1207.36 1656.91C1180.72 1661.6 1155.39 1671.98 1133.11 1687.34L768.557 1938.51C698.913 1986.48 603.729 1939.98 598.726 1855.86L572.502 1414.38C569.257 1359.74 542.373 1309.24 498.866 1276.01L147.203 1007.41C80.1767 956.216 94.8588 851.43 173.566 820.594L585.833 659.08C636.813 639.107 676.599 597.967 694.813 546.348L842.026 129.177Z" fill="#FFAFF3"/>
      <path d="M918.91 20.3875C868.969 29.1948 823.32 62.3526 804.42 115.904L657.186 533.07C642.831 573.741 611.52 606.123 571.327 621.871L159.044 783.395C53.2498 824.843 32.7918 970.356 122.981 1039.24L474.644 1307.81C491.576 1320.73 505.522 1337.15 515.528 1355.96C525.534 1374.76 531.366 1395.5 532.625 1416.76L558.835 1858.23C565.559 1971.49 697.668 2035.92 791.287 1971.44L791.289 1971.43L1155.87 1720.26L1155.87 1720.25C1173.42 1708.15 1193.38 1699.97 1214.37 1696.27C1235.37 1692.57 1256.92 1693.43 1277.55 1698.8L1277.55 1698.8L1706.04 1810.11C1816.06 1838.7 1918.17 1732.96 1885.75 1624.22L1885.75 1624.23L1759.42 1200.41L1759.42 1200.41C1753.33 1180 1751.72 1158.52 1754.69 1137.42C1757.66 1116.33 1765.15 1096.13 1776.65 1078.2L1776.65 1078.19L2015.25 705.513L2015.24 705.511C2076.44 609.933 2007.46 480.197 1893.87 477.434L1451.21 466.681C1408.06 465.633 1367.56 445.914 1340.17 412.608L1059.15 70.9554C1023.08 27.1014 968.841 11.5585 918.896 20.3665" fill="#151515"/>
      <path d="M790.631 1144.08C833.367 1136.54 861.904 1095.79 854.37 1053.06C846.836 1010.32 806.085 981.793 763.349 989.329C720.613 996.866 692.076 1037.62 699.61 1080.35C707.144 1123.08 747.896 1151.62 790.631 1144.08Z" fill="#151515"/>
      <path d="M1423.7 1032.45C1466.43 1024.92 1494.97 984.164 1487.43 941.432C1479.9 898.699 1439.15 870.166 1396.41 877.703C1353.68 885.239 1325.14 925.991 1332.67 968.724C1340.21 1011.46 1380.96 1039.99 1423.7 1032.45Z" fill="#151515"/>
      <path d="M1042.09 1126.57C1037.19 1128.46 1032.71 1131.3 1028.9 1134.92C1025.1 1138.54 1022.04 1142.88 1019.92 1147.68C1017.79 1152.49 1016.63 1157.66 1016.5 1162.91C1016.37 1168.16 1017.28 1173.39 1019.17 1178.29C1024.11 1191.06 1031.51 1202.74 1040.96 1212.66L1040.96 1212.66C1050.41 1222.58 1061.72 1230.53 1074.25 1236.07L1074.26 1236.08C1086.77 1241.62 1100.25 1244.64 1113.94 1244.97L1113.95 1244.97L1113.97 1244.97C1127.65 1245.29 1141.26 1242.92 1154.03 1237.99L1154.04 1237.99C1166.81 1233.06 1178.49 1225.66 1188.4 1216.21L1188.41 1216.21L1188.41 1216.2C1198.32 1206.76 1206.27 1195.45 1211.81 1182.94C1217.37 1170.41 1220.39 1156.9 1220.71 1143.2C1220.96 1132.6 1216.99 1122.33 1209.67 1114.65C1202.34 1106.98 1192.27 1102.53 1181.66 1102.28C1176.41 1102.15 1171.19 1103.07 1166.29 1104.96C1161.39 1106.86 1156.91 1109.7 1153.11 1113.33C1149.31 1116.95 1146.26 1121.29 1144.14 1126.1C1142.02 1130.9 1140.86 1136.08 1140.74 1141.33C1140.66 1144.5 1139.96 1147.63 1138.67 1150.53L1138.67 1150.54L1138.66 1150.56C1137.37 1153.47 1135.52 1156.1 1133.22 1158.3C1130.91 1160.5 1128.2 1162.22 1125.23 1163.36L1125.21 1163.37C1122.23 1164.52 1119.06 1165.07 1115.87 1165C1112.69 1164.92 1109.55 1164.21 1106.64 1162.92L1106.62 1162.92L1106.61 1162.91C1103.7 1161.62 1101.08 1159.78 1098.88 1157.48L1098.88 1157.47L1098.88 1157.47C1096.68 1155.16 1094.96 1152.45 1093.81 1149.47C1091.92 1144.57 1089.08 1140.09 1085.46 1136.29C1081.83 1132.49 1077.5 1129.43 1072.69 1127.31C1067.89 1125.18 1062.71 1124.02 1057.46 1123.89C1052.21 1123.76 1046.99 1124.67 1042.09 1126.57Z" fill="#151515"/>
    </svg>
  ),
  calculus: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="2" y="26" fontSize="28" fontFamily="Georgia, serif" fill="#A8B9CC">∫</text>
    </svg>
  ),
  calculus2: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="1" y="24" fontSize="22" fontFamily="Georgia, serif" fill="#A8B9CC">Σ</text>
    </svg>
  ),
  "linear-algebra": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <rect x="4" y="4" width="10" height="10" rx="1" stroke="#A8B9CC" strokeWidth="1.5" fill="none"/>
      <rect x="18" y="4" width="10" height="10" rx="1" stroke="#A8B9CC" strokeWidth="1.5" fill="none"/>
      <rect x="4" y="18" width="10" height="10" rx="1" stroke="#A8B9CC" strokeWidth="1.5" fill="none"/>
      <rect x="18" y="18" width="10" height="10" rx="1" stroke="#A8B9CC" strokeWidth="1.5" fill="none"/>
    </svg>
  ),
  diffeq: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="1" y="22" fontSize="14" fontFamily="Georgia, serif" fill="#A8B9CC">dy/dx</text>
    </svg>
  ),
  statistics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <rect x="4" y="20" width="5" height="8" rx="1" fill="#A8B9CC"/>
      <rect x="13" y="12" width="5" height="16" rx="1" fill="#A8B9CC"/>
      <rect x="22" y="6" width="5" height="22" rx="1" fill="#A8B9CC"/>
    </svg>
  ),
  probability: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <path d="M4 28 C4 28 8 4 16 16 C24 28 28 4 28 4" stroke="#A8B9CC" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"/>
      <circle cx="16" cy="16" r="2.5" fill="#A8B9CC"/>
    </svg>
  ),
  "discrete-math": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <circle cx="8" cy="8" r="3" fill="#A8B9CC"/>
      <circle cx="24" cy="8" r="3" fill="#A8B9CC"/>
      <circle cx="8" cy="24" r="3" fill="#A8B9CC"/>
      <circle cx="24" cy="24" r="3" fill="#A8B9CC"/>
      <line x1="8" y1="8" x2="24" y2="8" stroke="#A8B9CC" strokeWidth="1.5"/>
      <line x1="8" y1="8" x2="8" y2="24" stroke="#A8B9CC" strokeWidth="1.5"/>
      <line x1="24" y1="8" x2="24" y2="24" stroke="#A8B9CC" strokeWidth="1.5"/>
      <line x1="8" y1="24" x2="24" y2="24" stroke="#A8B9CC" strokeWidth="1.5"/>
      <line x1="8" y1="8" x2="24" y2="24" stroke="#A8B9CC" strokeWidth="1.5"/>
    </svg>
  ),
  python: (
    <svg viewBox="0 0 24 24" className="w-8 h-8">
      <path d="M11.914 0C5.82 0 6.2 2.656 6.2 2.656l.007 2.752h5.814v.826H3.9S0 5.789 0 11.969c0 6.18 3.403 5.96 3.403 5.96h2.031v-2.867s-.109-3.402 3.35-3.402h5.762s3.24.052 3.24-3.13V3.19S18.28 0 11.914 0zm-3.2 1.848a1.047 1.047 0 1 1 0 2.094 1.046 1.046 0 0 1 0-2.094z" fill="#387EB8"/>
      <path d="M12.086 24c6.094 0 5.714-2.656 5.714-2.656l-.007-2.752h-5.814v-.826h8.121S24 18.211 24 12.031c0-6.18-3.403-5.96-3.403-5.96h-2.031v2.867s.109 3.402-3.35 3.402H9.454s-3.24-.052-3.24 3.13V17.81S5.72 24 12.086 24zm3.2-1.848a1.047 1.047 0 1 1 0-2.094 1.046 1.046 0 0 1 0 2.094z" fill="#FFE052"/>
    </svg>
  ),
  "financial-math": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="3" y="24" fontSize="20" fontFamily="Georgia, serif" fill="#10b981">$</text>
      <path d="M14 8 L28 8" stroke="#10b981" strokeWidth="2" strokeLinecap="round"/>
      <path d="M14 14 L24 14" stroke="#10b981" strokeWidth="2" strokeLinecap="round"/>
      <path d="M14 20 L26 20" stroke="#10b981" strokeWidth="2" strokeLinecap="round"/>
    </svg>
  ),
  "quant-stats": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <path d="M4 28 Q8 10 12 18 Q16 26 20 12 Q24 2 28 8" stroke="#10b981" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"/>
    </svg>
  ),
  "time-series": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <polyline points="4,24 8,18 12,20 16,12 20,16 24,8 28,10" stroke="#10b981" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"/>
      <line x1="4" y1="28" x2="28" y2="28" stroke="#10b981" strokeWidth="1.5" strokeLinecap="round"/>
    </svg>
  ),
  "portfolio-theory": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <circle cx="16" cy="16" r="10" stroke="#10b981" strokeWidth="1.5"/>
      <path d="M16 6 L22 22 L10 14 L22 14 L10 22 Z" stroke="#10b981" strokeWidth="1.5" strokeLinejoin="round"/>
    </svg>
  ),
  "options-pricing": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <path d="M4 26 Q10 26 14 20 Q18 14 28 6" stroke="#10b981" strokeWidth="2" strokeLinecap="round"/>
      <line x1="4" y1="6" x2="4" y2="28" stroke="#10b981" strokeWidth="1.5" strokeLinecap="round"/>
      <line x1="4" y1="28" x2="28" y2="28" stroke="#10b981" strokeWidth="1.5" strokeLinecap="round"/>
    </svg>
  ),
  "algo-trading": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <rect x="4" y="18" width="5" height="10" rx="1" fill="#10b981"/>
      <rect x="11" y="12" width="5" height="16" rx="1" fill="#10b981"/>
      <rect x="18" y="6" width="5" height="22" rx="1" fill="#10b981"/>
      <polyline points="6,16 13,10 20,4" stroke="#10b981" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/>
    </svg>
  ),
  "risk-management": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <path d="M16 4 L28 10 L28 20 C28 25 22 29 16 30 C10 29 4 25 4 20 L4 10 Z" stroke="#10b981" strokeWidth="1.5" strokeLinejoin="round"/>
      <line x1="16" y1="13" x2="16" y2="19" stroke="#10b981" strokeWidth="2" strokeLinecap="round"/>
      <circle cx="16" cy="22" r="1.2" fill="#10b981"/>
    </svg>
  ),
  // Languages
  rust: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="#CE422B">
      <path d="M16 3C9 3 3 9 3 16s6 13 13 13 13-6 13-13S23 3 16 3zm0 24C10.5 27 6 22.5 6 17S10.5 7 16 7s10 4.5 10 10-4.5 10-10 10z"/>
      <circle cx="16" cy="17" r="3.5"/>
      <rect x="14.5" y="0" width="3" height="5" rx="1.5"/>
      <rect x="14.5" y="27" width="3" height="5" rx="1.5"/>
      <rect x="0" y="14.5" width="5" height="3" rx="1.5"/>
      <rect x="27" y="14.5" width="5" height="3" rx="1.5"/>
      <rect x="4.5" y="3" width="3" height="5" rx="1.5" transform="rotate(45 6 5.5)"/>
      <rect x="24.5" y="3" width="3" height="5" rx="1.5" transform="rotate(-45 26 5.5)"/>
      <rect x="4.5" y="24" width="3" height="5" rx="1.5" transform="rotate(-45 6 26.5)"/>
      <rect x="24.5" y="24" width="3" height="5" rx="1.5" transform="rotate(45 26 26.5)"/>
    </svg>
  ),
  cpp: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="4" fill="#00599C"/>
      <text x="3" y="22" fontSize="13" fontFamily="monospace" fill="white" fontWeight="bold">C++</text>
    </svg>
  ),
  java: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#5382A1"/>
      <text x="4" y="22" fontSize="12" fontFamily="monospace" fill="#F89820" fontWeight="bold">Java</text>
    </svg>
  ),
  kotlin: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <defs>
        <linearGradient id="ktGrad" x1="0" y1="0" x2="1" y2="1">
          <stop offset="0%" stopColor="#E44857"/>
          <stop offset="50%" stopColor="#C711E1"/>
          <stop offset="100%" stopColor="#7F52FF"/>
        </linearGradient>
      </defs>
      <rect width="32" height="32" rx="3" fill="url(#ktGrad)"/>
      <polygon points="3,3 16,16 3,29" fill="white" opacity="0.9"/>
      <polygon points="3,3 29,3 16,16" fill="white" opacity="0.7"/>
      <polygon points="16,16 29,3 29,29" fill="white" opacity="0.85"/>
    </svg>
  ),
  csharp: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#9B4F96"/>
      <text x="4" y="22" fontSize="13" fontFamily="monospace" fill="white" fontWeight="bold">C#</text>
    </svg>
  ),
  swift: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <rect width="32" height="32" rx="3" fill="#F05138"/>
      <text x="3" y="22" fontSize="10" fontFamily="monospace" fill="white" fontWeight="bold">Swift</text>
    </svg>
  ),
  elixir: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <defs>
        <linearGradient id="elixirGrad" x1="0" y1="0" x2="1" y2="1">
          <stop offset="0%" stopColor="#9B59B6"/>
          <stop offset="100%" stopColor="#6E4AFF"/>
        </linearGradient>
      </defs>
      <ellipse cx="16" cy="17" rx="10" ry="13" fill="url(#elixirGrad)"/>
      <ellipse cx="16" cy="9" rx="6" ry="8" fill="#B07FFF" opacity="0.7"/>
    </svg>
  ),
  scala: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <defs>
        <linearGradient id="scalaGrad" x1="0" y1="0" x2="0" y2="1">
          <stop offset="0%" stopColor="#DC322F"/>
          <stop offset="100%" stopColor="#7B1616"/>
        </linearGradient>
      </defs>
      <rect x="6" y="6" width="20" height="6" rx="1" fill="url(#scalaGrad)"/>
      <rect x="6" y="13" width="20" height="6" rx="1" fill="url(#scalaGrad)" opacity="0.75"/>
      <rect x="6" y="20" width="20" height="6" rx="1" fill="url(#scalaGrad)" opacity="0.5"/>
    </svg>
  ),
  ruby: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <polygon points="16,3 27,11 27,21 16,29 5,21 5,11" fill="#CC342D"/>
      <polygon points="16,3 27,11 16,16" fill="#E86060" opacity="0.5"/>
      <polygon points="16,16 5,11 5,21 16,29" fill="#7B1010" opacity="0.5"/>
      <line x1="5" y1="11" x2="27" y2="11" stroke="#FF9090" strokeWidth="0.8" opacity="0.4"/>
    </svg>
  ),
  haskell: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="3" y="26" fontSize="26" fontFamily="Georgia, serif" fill="#5D4F85">λ</text>
    </svg>
  ),
  holyc: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <rect x="4" y="6" width="24" height="2.5" rx="1" fill="#FFD700"/>
      <rect x="4" y="23.5" width="24" height="2.5" rx="1" fill="#FFD700"/>
      <rect x="5" y="8.5" width="3.5" height="15" rx="1" fill="#FFD700"/>
      <rect x="14.25" y="8.5" width="3.5" height="15" rx="1" fill="#FFD700"/>
      <rect x="23.5" y="8.5" width="3.5" height="15" rx="1" fill="#FFD700"/>
    </svg>
  ),
  // Web & Frontend
  html: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#E44D26"/>
      <text x="4" y="22" fontSize="11" fontFamily="monospace" fill="white" fontWeight="bold">HTML</text>
    </svg>
  ),
  css: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#1572B6"/>
      <text x="5" y="22" fontSize="11" fontFamily="monospace" fill="white" fontWeight="bold">CSS</text>
    </svg>
  ),
  tailwind: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <path d="M16 6c-3.2 0-5.2 1.6-6 4.8 1.2-1.6 2.6-2.2 4.2-1.8.9.23 1.55.9 2.27 1.64C17.68 11.9 19.1 13.4 22 13.4c3.2 0 5.2-1.6 6-4.8-1.2 1.6-2.6 2.2-4.2 1.8-.9-.23-1.55-.9-2.27-1.64C20.32 7.5 18.9 6 16 6zm-6 8c-3.2 0-5.2 1.6-6 4.8 1.2-1.6 2.6-2.2 4.2-1.8.9.23 1.55.9 2.27 1.64C11.68 19.9 13.1 21.4 16 21.4c3.2 0 5.2-1.6 6-4.8-1.2 1.6-2.6 2.2-4.2 1.8-.9-.23-1.55-.9-2.27-1.64C14.32 15.5 12.9 14 10 14z" fill="#38BDF8"/>
    </svg>
  ),
  javascript: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#F7DF1E"/>
      <path d="M8 22.5c.5 1 1.3 1.5 2.5 1.5 1.2 0 2-.6 2-1.8V15h2.5v7.4c0 2.7-1.6 4-3.9 4-2.1 0-3.3-1.1-3.9-2.5zM17.5 22c.6 1 1.6 1.7 3 1.7 1.3 0 2.2-.6 2.2-1.6 0-1-.8-1.4-2.2-2l-.7-.3c-2.2-.9-3.6-2-3.6-4.4 0-2.2 1.7-3.9 4.4-3.9 1.9 0 3.2.7 4.2 2.4l-2.2 1.4c-.5-.9-1-1.2-1.9-1.2-.9 0-1.5.5-1.5 1.2 0 .8.5 1.2 1.8 1.7l.7.3c2.5 1.1 3.9 2.2 3.9 4.6 0 2.7-2 4.1-4.8 4.1-2.7 0-4.5-1.3-5.3-3z" fill="#000"/>
    </svg>
  ),
  typescript: (
    <svg viewBox="0 0 32 32" className="w-8 h-8">
      <rect width="32" height="32" rx="3" fill="#3178C6"/>
      <path d="M5 15h9v2.5H11V27H8.5V17.5H5zM18.5 14.8c-1.6 0-3.1.9-3.1 2.5 0 1.8 1.2 2.5 3.1 3.2.6.2 1.2.5 1.6.8.4.3.5.7.5 1 0 .9-.8 1.4-1.8 1.4-1.3 0-2.2-.7-2.8-1.7l-2.2 1.2c.9 1.8 2.6 2.8 5 2.8 2.8 0 4.8-1.5 4.8-4 0-1.9-1.1-3-3.1-3.8-.7-.3-1.3-.5-1.7-.8-.4-.2-.6-.5-.6-.9 0-.7.5-1 1.4-1s1.7.5 2.3 1.4l2.1-1.3c-.9-1.5-2.3-1.8-3.5-1.8z" fill="white"/>
    </svg>
  ),
  threejs: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <polygon points="16,3 29,24 3,24"/>
      <polygon points="16,9 24,24 8,24" opacity="0.45"/>
      <line x1="16" y1="3" x2="16" y2="9"/>
    </svg>
  ),
  music: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="currentColor">
      <ellipse cx="11" cy="24" rx="4.5" ry="3" transform="rotate(-10 11 24)"/>
      <ellipse cx="24" cy="21" rx="4.5" ry="3" transform="rotate(-10 24 21)"/>
      <rect x="14.5" y="7" width="2" height="17.5" rx="1"/>
      <rect x="27.5" y="4" width="2" height="17.5" rx="1"/>
      <rect x="14.5" y="7" width="15" height="2" rx="1"/>
      <rect x="14.5" y="11" width="15" height="2" rx="1"/>
    </svg>
  ),
  // Data & Databases
  mysql: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <ellipse cx="16" cy="8" rx="11" ry="4" fill="#5B9BD5"/>
      <rect x="5" y="8" width="22" height="10" fill="#4479A1"/>
      <ellipse cx="16" cy="18" rx="11" ry="4" fill="#5B9BD5"/>
      <rect x="5" y="14" width="22" height="4" fill="#4479A1"/>
      <ellipse cx="16" cy="8" rx="11" ry="4" fill="#5B9BD5"/>
      <text x="10" y="16" fontSize="7" fontFamily="monospace" fill="white" fontWeight="bold">MY</text>
    </svg>
  ),
  sqlite: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <ellipse cx="13" cy="7" rx="9" ry="3.5" fill="#0F6197"/>
      <rect x="4" y="7" width="18" height="16" fill="#003B57"/>
      <ellipse cx="13" cy="23" rx="9" ry="3.5" fill="#0F6197"/>
      <ellipse cx="13" cy="7" rx="9" ry="3.5" fill="#1A8BBD"/>
      <path d="M22 12 Q28 12 28 7 Q28 2 22 2" stroke="#4FC3F7" strokeWidth="1.5" fill="none" strokeLinecap="round"/>
    </svg>
  ),
  redis: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <ellipse cx="16" cy="24" rx="13" ry="4.5" fill="#A31515"/>
      <rect x="3" y="18" width="26" height="6" fill="#DC382D"/>
      <ellipse cx="16" cy="18" rx="13" ry="4.5" fill="#FF6B6B"/>
      <rect x="3" y="12" width="26" height="6" fill="#DC382D"/>
      <ellipse cx="16" cy="12" rx="13" ry="4.5" fill="#FF8C8C"/>
    </svg>
  ),
  // Algorithms & CS
  algorithms: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="currentColor">
      <rect x="3" y="22" width="5" height="7" rx="1"/>
      <rect x="10" y="16" width="5" height="13" rx="1"/>
      <rect x="17" y="10" width="5" height="19" rx="1"/>
      <rect x="24" y="14" width="5" height="15" rx="1"/>
      <polyline points="5,20 12,14 19,8 26,12" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" fill="none"/>
    </svg>
  ),
  graphs: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="6" r="3" fill="currentColor" stroke="none"/>
      <circle cx="6" cy="22" r="3" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="22" r="3" fill="currentColor" stroke="none"/>
      <circle cx="16" cy="16" r="3" fill="currentColor" stroke="none" opacity="0.6"/>
      <line x1="16" y1="9" x2="6" y2="19"/>
      <line x1="16" y1="9" x2="26" y2="19"/>
      <line x1="9" y1="22" x2="23" y2="22"/>
      <line x1="16" y1="9" x2="16" y2="13"/>
      <line x1="16" y1="19" x2="9" y2="21"/>
      <line x1="16" y1="19" x2="23" y2="21"/>
    </svg>
  ),
  trees: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="5" r="3" fill="currentColor" stroke="none"/>
      <circle cx="9" cy="15" r="3" fill="currentColor" stroke="none"/>
      <circle cx="23" cy="15" r="3" fill="currentColor" stroke="none"/>
      <circle cx="5" cy="25" r="2.5" fill="currentColor" stroke="none" opacity="0.7"/>
      <circle cx="13" cy="25" r="2.5" fill="currentColor" stroke="none" opacity="0.7"/>
      <circle cx="23" cy="25" r="2.5" fill="currentColor" stroke="none" opacity="0.7"/>
      <line x1="16" y1="8" x2="9" y2="12"/>
      <line x1="16" y1="8" x2="23" y2="12"/>
      <line x1="9" y1="18" x2="5" y2="22.5"/>
      <line x1="9" y1="18" x2="13" y2="22.5"/>
      <line x1="23" y1="18" x2="23" y2="22.5"/>
    </svg>
  ),
  "linked-lists": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <rect x="1" y="12" width="8" height="8" rx="1.5"/>
      <rect x="12" y="12" width="8" height="8" rx="1.5"/>
      <rect x="23" y="12" width="8" height="8" rx="1.5"/>
      <line x1="9" y1="16" x2="12" y2="16"/>
      <line x1="20" y1="16" x2="23" y2="16"/>
      <polyline points="21,14 23,16 21,18"/>
      <polyline points="10,14 12,16 10,18"/>
    </svg>
  ),
  "distributed-systems": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="6" r="4" fill="currentColor" fillOpacity="0.2"/>
      <circle cx="5" cy="25" r="4" fill="currentColor" fillOpacity="0.2"/>
      <circle cx="27" cy="25" r="4" fill="currentColor" fillOpacity="0.2"/>
      <line x1="16" y1="10" x2="5" y2="21"/>
      <line x1="16" y1="10" x2="27" y2="21"/>
      <line x1="9" y1="25" x2="23" y2="25"/>
    </svg>
  ),
  raytracer: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="22" cy="20" r="7"/>
      <line x1="2" y1="6" x2="16" y2="14.5"/>
      <line x1="2" y1="6" x2="15.5" y2="16.5"/>
      <line x1="2" y1="6" x2="15.5" y2="18.5"/>
      <circle cx="2" cy="6" r="1.5" fill="currentColor" stroke="none"/>
    </svg>
  ),
  // Mathematics
  calculus3: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="2" y="26" fontSize="26" fontFamily="Georgia, serif" fill="#A8B9CC">∇</text>
    </svg>
  ),
  "advanced-linear-algebra": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="#A8B9CC" strokeWidth="1.5">
      <rect x="4" y="4" width="11" height="11" rx="1"/>
      <rect x="17" y="4" width="11" height="11" rx="1"/>
      <rect x="4" y="17" width="11" height="11" rx="1"/>
      <rect x="17" y="17" width="11" height="11" rx="1"/>
      <text x="7.5" y="13" fontSize="8" fontFamily="Georgia, serif" fill="#A8B9CC" stroke="none">λ</text>
    </svg>
  ),
  "number-theory": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="2" y="26" fontSize="22" fontFamily="Georgia, serif" fill="#A8B9CC">ℤ</text>
    </svg>
  ),
  cryptography: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <rect x="8" y="14" width="16" height="13" rx="2"/>
      <path d="M11 14v-3a5 5 0 0 1 10 0v3"/>
      <circle cx="16" cy="20" r="2" fill="currentColor" stroke="none"/>
      <line x1="16" y1="22" x2="16" y2="25"/>
    </svg>
  ),
  "signal-processing": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="#A8B9CC" strokeWidth="1.5" strokeLinecap="round">
      <path d="M2 16 Q6 8 10 16 Q14 24 18 16 Q22 8 26 16 Q28 20 30 16"/>
      <rect x="6" y="23" width="4" height="5" rx="0.5" fill="#A8B9CC" stroke="none"/>
      <rect x="14" y="25" width="4" height="3" rx="0.5" fill="#A8B9CC" stroke="none"/>
      <rect x="22" y="24" width="4" height="4" rx="0.5" fill="#A8B9CC" stroke="none"/>
    </svg>
  ),
  "information-theory": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="2" y="24" fontSize="14" fontFamily="monospace" fill="#A8B9CC">H(X)</text>
    </svg>
  ),
  "functional-diff-geo": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="#A8B9CC" strokeWidth="1.5" strokeLinecap="round">
      <path d="M3 22 Q8 8 16 14 Q24 20 29 10"/>
      <path d="M3 26 Q8 12 16 18 Q24 24 29 14" opacity="0.4"/>
      <line x1="3" y1="22" x2="3" y2="26" opacity="0.3"/>
      <line x1="16" y1="14" x2="16" y2="18" opacity="0.3"/>
      <line x1="29" y1="10" x2="29" y2="14" opacity="0.3"/>
    </svg>
  ),
  // Classical Physics
  "classical-mechanics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <line x1="4" y1="4" x2="20" y2="4"/>
      <line x1="12" y1="4" x2="22" y2="22"/>
      <circle cx="22" cy="24" r="3" fill="currentColor" fillOpacity="0.3"/>
      <path d="M8 14 Q16 18 22 22" strokeDasharray="3,2" opacity="0.5"/>
    </svg>
  ),
  thermodynamics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <rect x="13" y="5" width="6" height="16" rx="3"/>
      <circle cx="16" cy="24" r="4"/>
      <rect x="14" y="16" width="4" height="5" fill="currentColor" fillOpacity="0.4" stroke="none"/>
      <circle cx="16" cy="24" r="4" fill="currentColor" fillOpacity="0.3" stroke="none"/>
      <line x1="19" y1="9" x2="22" y2="9" strokeWidth="1"/>
      <line x1="19" y1="12" x2="22" y2="12" strokeWidth="1"/>
      <line x1="19" y1="15" x2="22" y2="15" strokeWidth="1"/>
    </svg>
  ),
  waves: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M2 16 Q6 6 10 16 Q14 26 18 16 Q22 6 26 16 Q28 21 30 16"/>
      <path d="M2 22 Q5 16 8 22 Q11 28 14 22 Q17 16 20 22 Q23 28 26 22" opacity="0.35"/>
    </svg>
  ),
  electromagnetism: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M4 16 C4 8 12 8 16 16 C20 24 28 24 28 16"/>
      <path d="M4 16 C4 24 12 24 16 16 C20 8 28 8 28 16" opacity="0.4"/>
      <circle cx="4" cy="16" r="1.5" fill="currentColor" stroke="none"/>
      <circle cx="28" cy="16" r="1.5" fill="currentColor" stroke="none"/>
    </svg>
  ),
  circuits: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M4 16 H9"/>
      <rect x="9" y="13" width="4" height="6" rx="0.5"/>
      <path d="M13 16 H16"/>
      <rect x="16" y="13" width="4" height="6" rx="0.5"/>
      <path d="M20 16 H28"/>
      <path d="M4 16 L4 24 L28 24 L28 16"/>
      <circle cx="4" cy="16" r="1.5" fill="currentColor" stroke="none"/>
    </svg>
  ),
  "digital-logic": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <rect x="10" y="9" width="12" height="14" rx="1"/>
      <path d="M6 13 H10"/>
      <path d="M6 16 H10"/>
      <path d="M6 19 H10"/>
      <path d="M22 13 H26"/>
      <path d="M22 19 H26"/>
    </svg>
  ),
  "pcb-design": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <rect x="3" y="3" width="26" height="26" rx="2"/>
      <rect x="10" y="10" width="12" height="12" rx="1"/>
      <path d="M10 7 L10 3"/>
      <path d="M16 7 L16 3"/>
      <path d="M22 7 L22 3"/>
      <path d="M10 25 L10 29"/>
      <path d="M22 25 L22 29"/>
      <path d="M3 16 L7 16"/>
      <path d="M25 16 L29 16"/>
    </svg>
  ),
  "fluid-mechanics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M4 10 Q8 14 12 10 Q16 6 20 10 Q24 14 28 10"/>
      <path d="M4 17 Q8 21 12 17 Q16 13 20 17 Q24 21 28 17"/>
      <path d="M4 24 Q8 28 12 24 Q16 20 20 24 Q24 28 28 24"/>
      <polyline points="26,14.5 28,17 26,19.5" strokeWidth="1.2"/>
    </svg>
  ),
  optics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <ellipse cx="14" cy="16" rx="3" ry="11"/>
      <line x1="2" y1="11" x2="11.5" y2="13.5"/>
      <line x1="2" y1="16" x2="11" y2="16"/>
      <line x1="2" y1="21" x2="11.5" y2="18.5"/>
      <line x1="16.5" y1="13.5" x2="28" y2="9"/>
      <line x1="17" y1="16" x2="28" y2="16"/>
      <line x1="16.5" y1="18.5" x2="28" y2="23"/>
    </svg>
  ),
  // Modern & Theoretical Physics
  "special-relativity": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <line x1="16" y1="2" x2="16" y2="30"/>
      <line x1="2" y1="16" x2="30" y2="16"/>
      <line x1="16" y1="16" x2="4" y2="4" strokeDasharray="3,2"/>
      <line x1="16" y1="16" x2="28" y2="4" strokeDasharray="3,2"/>
      <line x1="16" y1="16" x2="4" y2="28" strokeDasharray="3,2" opacity="0.4"/>
      <line x1="16" y1="16" x2="28" y2="28" strokeDasharray="3,2" opacity="0.4"/>
    </svg>
  ),
  "general-relativity": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M4 8 Q10 10 16 16 Q22 22 28 24"/>
      <path d="M4 16 Q10 16 16 16 Q22 16 28 16" opacity="0.35"/>
      <path d="M4 24 Q10 22 16 16 Q22 10 28 8"/>
      <path d="M8 4 Q10 10 16 16 Q22 22 24 28" opacity="0.35"/>
      <path d="M16 4 Q16 10 16 16 Q16 22 16 28" opacity="0.35"/>
      <path d="M24 4 Q22 10 16 16 Q10 22 8 28" opacity="0.35"/>
      <circle cx="16" cy="16" r="3" fill="currentColor" fillOpacity="0.4" stroke="none"/>
    </svg>
  ),
  quantum: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="16" r="3"/>
      <ellipse cx="16" cy="16" rx="13" ry="5"/>
      <ellipse cx="16" cy="16" rx="13" ry="5" transform="rotate(60 16 16)"/>
      <ellipse cx="16" cy="16" rx="13" ry="5" transform="rotate(-60 16 16)"/>
      <circle cx="16" cy="16" r="1.8" fill="currentColor" stroke="none"/>
    </svg>
  ),
  "advanced-quantum": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="2" y="22" fontSize="16" fontFamily="Georgia, serif" fill="#A8B9CC">|ψ⟩</text>
    </svg>
  ),
  "nuclear-physics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="16" r="5" fill="currentColor" fillOpacity="0.25"/>
      <circle cx="14" cy="14" r="2" fill="currentColor" stroke="none" opacity="0.9"/>
      <circle cx="18" cy="14" r="2" fill="currentColor" stroke="none" opacity="0.7"/>
      <circle cx="16" cy="18" r="2" fill="currentColor" stroke="none" opacity="0.8"/>
      <ellipse cx="16" cy="16" rx="13" ry="5" opacity="0.4"/>
      <ellipse cx="16" cy="16" rx="13" ry="5" transform="rotate(65 16 16)" opacity="0.3"/>
    </svg>
  ),
  "particle-physics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <line x1="4" y1="12" x2="16" y2="16"/>
      <line x1="4" y1="20" x2="16" y2="16"/>
      <line x1="16" y1="16" x2="28" y2="9"/>
      <line x1="16" y1="16" x2="28" y2="23"/>
      <path d="M16 16 Q20 11 25 16" strokeDasharray="2,2"/>
      <circle cx="16" cy="16" r="2" fill="currentColor" stroke="none"/>
    </svg>
  ),
  "plasma-physics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M19 3 L13 14 L18 14 L13 29 L26 13 L20 13 Z"/>
    </svg>
  ),
  "condensed-matter": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.2" strokeLinecap="round">
      <circle cx="6" cy="6" r="2" fill="currentColor" stroke="none"/>
      <circle cx="16" cy="6" r="2" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="6" r="2" fill="currentColor" stroke="none"/>
      <circle cx="6" cy="16" r="2" fill="currentColor" stroke="none"/>
      <circle cx="16" cy="16" r="2" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="16" r="2" fill="currentColor" stroke="none"/>
      <circle cx="6" cy="26" r="2" fill="currentColor" stroke="none"/>
      <circle cx="16" cy="26" r="2" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="26" r="2" fill="currentColor" stroke="none"/>
      <line x1="8" y1="6" x2="14" y2="6"/>
      <line x1="18" y1="6" x2="24" y2="6"/>
      <line x1="8" y1="16" x2="14" y2="16"/>
      <line x1="18" y1="16" x2="24" y2="16"/>
      <line x1="8" y1="26" x2="14" y2="26"/>
      <line x1="18" y1="26" x2="24" y2="26"/>
      <line x1="6" y1="8" x2="6" y2="14"/>
      <line x1="16" y1="8" x2="16" y2="14"/>
      <line x1="26" y1="8" x2="26" y2="14"/>
      <line x1="6" y1="18" x2="6" y2="24"/>
      <line x1="16" y1="18" x2="16" y2="24"/>
      <line x1="26" y1="18" x2="26" y2="24"/>
    </svg>
  ),
  biophysics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M10 3 Q20 9 10 15 Q20 21 10 27"/>
      <path d="M22 3 Q12 9 22 15 Q12 21 22 27"/>
      <line x1="10" y1="7" x2="22" y2="7" strokeWidth="1.2" opacity="0.6"/>
      <line x1="10" y1="13" x2="22" y2="13" strokeWidth="1.2" opacity="0.6"/>
      <line x1="10" y1="19" x2="22" y2="19" strokeWidth="1.2" opacity="0.6"/>
      <line x1="10" y1="25" x2="22" y2="25" strokeWidth="1.2" opacity="0.6"/>
    </svg>
  ),
  "mathematical-physics": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none">
      <text x="4" y="26" fontSize="22" fontFamily="Georgia, serif" fill="#A8B9CC">∂</text>
    </svg>
  ),
  // Science & AI
  "machine-learning": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round">
      <circle cx="5" cy="10" r="2.5" fill="currentColor" fillOpacity="0.3"/>
      <circle cx="5" cy="22" r="2.5" fill="currentColor" fillOpacity="0.3"/>
      <circle cx="16" cy="6" r="2.5" fill="currentColor" fillOpacity="0.5"/>
      <circle cx="16" cy="16" r="2.5" fill="currentColor" fillOpacity="0.5"/>
      <circle cx="16" cy="26" r="2.5" fill="currentColor" fillOpacity="0.5"/>
      <circle cx="27" cy="11" r="2.5" fill="currentColor" fillOpacity="0.7"/>
      <circle cx="27" cy="21" r="2.5" fill="currentColor" fillOpacity="0.7"/>
      <line x1="7.5" y1="10" x2="13.5" y2="7"/>
      <line x1="7.5" y1="10" x2="13.5" y2="16"/>
      <line x1="7.5" y1="10" x2="13.5" y2="25"/>
      <line x1="7.5" y1="22" x2="13.5" y2="7"/>
      <line x1="7.5" y1="22" x2="13.5" y2="16"/>
      <line x1="7.5" y1="22" x2="13.5" y2="25"/>
      <line x1="18.5" y1="7" x2="24.5" y2="12"/>
      <line x1="18.5" y1="7" x2="24.5" y2="21"/>
      <line x1="18.5" y1="16" x2="24.5" y2="12"/>
      <line x1="18.5" y1="16" x2="24.5" y2="21"/>
      <line x1="18.5" y1="25" x2="24.5" y2="12"/>
      <line x1="18.5" y1="25" x2="24.5" y2="21"/>
    </svg>
  ),
  microgpt: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round" strokeLinejoin="round">
      <rect x="3" y="22" width="7" height="7" rx="1.5"/>
      <rect x="13" y="22" width="7" height="7" rx="1.5"/>
      <rect x="23" y="22" width="7" height="7" rx="1.5"/>
      <rect x="8" y="12" width="7" height="7" rx="1.5"/>
      <rect x="18" y="12" width="7" height="7" rx="1.5"/>
      <rect x="13" y="3" width="7" height="7" rx="1.5"/>
      <line x1="6.5" y1="22" x2="11.5" y2="19"/>
      <line x1="16.5" y1="22" x2="11.5" y2="19"/>
      <line x1="16.5" y1="22" x2="21.5" y2="19"/>
      <line x1="26.5" y1="22" x2="21.5" y2="19"/>
      <line x1="11.5" y1="12" x2="16.5" y2="10"/>
      <line x1="21.5" y1="12" x2="16.5" y2="10"/>
    </svg>
  ),
  genomics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M11 3 Q21 9 11 15 Q21 21 11 27"/>
      <path d="M21 3 Q11 9 21 15 Q11 21 21 27"/>
      <line x1="11" y1="6" x2="21" y2="6" strokeWidth="1.2" opacity="0.7"/>
      <line x1="11" y1="12" x2="21" y2="12" strokeWidth="1.2" opacity="0.7"/>
      <line x1="11" y1="18" x2="21" y2="18" strokeWidth="1.2" opacity="0.7"/>
      <line x1="11" y1="24" x2="21" y2="24" strokeWidth="1.2" opacity="0.7"/>
    </svg>
  ),
  cosmology: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5">
      <circle cx="16" cy="16" r="13"/>
      <ellipse cx="16" cy="16" rx="13" ry="5"/>
      <circle cx="10" cy="10" r="1" fill="currentColor" stroke="none"/>
      <circle cx="22" cy="8" r="1.5" fill="currentColor" stroke="none"/>
      <circle cx="21" cy="22" r="1" fill="currentColor" stroke="none"/>
      <circle cx="8" cy="22" r="1" fill="currentColor" stroke="none"/>
      <circle cx="16" cy="5" r="0.8" fill="currentColor" stroke="none"/>
    </svg>
  ),
  astrophysics: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="16" r="5" fill="currentColor" fillOpacity="0.25"/>
      <line x1="16" y1="3" x2="16" y2="8"/>
      <line x1="16" y1="24" x2="16" y2="29"/>
      <line x1="3" y1="16" x2="8" y2="16"/>
      <line x1="24" y1="16" x2="29" y2="16"/>
      <line x1="7" y1="7" x2="10.5" y2="10.5"/>
      <line x1="25" y1="7" x2="21.5" y2="10.5"/>
      <line x1="7" y1="25" x2="10.5" y2="21.5"/>
      <line x1="25" y1="25" x2="21.5" y2="21.5"/>
    </svg>
  ),
  "complex-systems": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.2" strokeLinecap="round">
      <circle cx="16" cy="16" r="2" fill="currentColor" stroke="none"/>
      <circle cx="6" cy="10" r="2" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="8" r="2" fill="currentColor" stroke="none"/>
      <circle cx="10" cy="26" r="2" fill="currentColor" stroke="none"/>
      <circle cx="26" cy="24" r="2" fill="currentColor" stroke="none"/>
      <circle cx="4" cy="22" r="2" fill="currentColor" stroke="none"/>
      <circle cx="22" cy="4" r="2" fill="currentColor" stroke="none"/>
      <line x1="16" y1="16" x2="6" y2="10"/>
      <line x1="16" y1="16" x2="26" y2="8"/>
      <line x1="16" y1="16" x2="10" y2="26"/>
      <line x1="16" y1="16" x2="26" y2="24"/>
      <line x1="16" y1="16" x2="4" y2="22"/>
      <line x1="16" y1="16" x2="22" y2="4"/>
      <line x1="6" y1="10" x2="22" y2="4"/>
      <line x1="4" y1="22" x2="10" y2="26"/>
      <line x1="26" y1="24" x2="10" y2="26"/>
    </svg>
  ),
  "chaos-theory": (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <path d="M16 16 C10 10 4 6 8 14 C12 22 16 16 16 16"/>
      <path d="M16 16 C22 10 28 6 24 14 C20 22 16 16 16 16"/>
      <circle cx="16" cy="16" r="1.5" fill="currentColor" stroke="none"/>
    </svg>
  ),
  // Systems
  linux: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <rect x="3" y="5" width="26" height="20" rx="3"/>
      <line x1="3" y1="25" x2="29" y2="25"/>
      <path d="M8 14 L12 17 L8 20"/>
      <line x1="13" y1="20" x2="20" y2="20"/>
    </svg>
  ),
  kernel: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <circle cx="16" cy="16" r="3"/>
      <circle cx="16" cy="16" r="7"/>
      <circle cx="16" cy="16" r="12"/>
    </svg>
  ),
  coreutils: (
    <svg viewBox="0 0 32 32" className="w-8 h-8" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round">
      <rect x="3" y="5" width="26" height="22" rx="3"/>
      <line x1="8" y1="12" x2="18" y2="12"/>
      <line x1="8" y1="16" x2="22" y2="16"/>
      <line x1="8" y1="20" x2="14" y2="20"/>
      <polyline points="19,18 22,16 19,14" strokeWidth="1.2"/>
    </svg>
  ),
};

// Tracks where courses should be taken in order (ids listed from first to last)
const orderedCurriculums: { title: string; subtitle: string; ids: string[] }[] = [
  {
    title: "Systems & Low-Level",
    subtitle: "Understand how computers work at the metal — from ARM64 assembly to a C compiler, Linux shell, and kernel primitives.",
    ids: ["arm64", "c", "linux", "kernel", "coreutils"],
  },
  {
    title: "Mathematics in C",
    subtitle: "Implement university mathematics in C — from limits and integrals to Taylor series and curvature. Each course builds on the previous.",
    ids: ["calculus", "calculus2", "calculus3"],
  },
  {
    title: "Quantitative Finance",
    subtitle: "Build the complete quantitative finance toolkit from scratch in Python — from time value of money to live trading strategies.",
    ids: [
      "financial-math",
      "quant-stats",
      "time-series",
      "portfolio-theory",
      "options-pricing",
      "algo-trading",
      "risk-management",
    ],
  },
];

// Unordered tracks — take any course in any order
const tracks: { title: string; ids: string[] }[] = [
  {
    title: "Languages",
    ids: ["go", "zig", "rust", "cpp", "java", "kotlin", "csharp", "swift", "elixir", "scala", "python", "gleam", "ruby", "haskell", "lean", "holyc"],
  },
  {
    title: "Web & Frontend",
    ids: ["html", "css", "tailwind", "javascript", "typescript", "threejs", "music"],
  },
  {
    title: "Data & Databases",
    ids: ["postgresql", "mysql", "sqlite", "redis", "r"],
  },
  {
    title: "Algorithms & Computer Science",
    ids: [
      "algorithms",
      "graphs",
      "trees",
      "linked-lists",
      "distributed-systems",
      "raytracer",
    ],
  },
  {
    title: "Mathematics in Python",
    ids: [
      "linear-algebra",
      "advanced-linear-algebra",
      "statistics",
      "probability",
      "discrete-math",
      "diffeq",
      "number-theory",
      "cryptography",
      "signal-processing",
      "information-theory",
      "functional-diff-geo",
    ],
  },
  {
    title: "Classical Physics & Electronics",
    ids: [
      "classical-mechanics",
      "thermodynamics",
      "waves",
      "electromagnetism",
      "circuits",
      "digital-logic",
      "pcb-design",
      "fluid-mechanics",
      "optics",
    ],
  },
  {
    title: "Modern & Theoretical Physics",
    ids: [
      "special-relativity",
      "general-relativity",
      "quantum",
      "advanced-quantum",
      "nuclear-physics",
      "particle-physics",
      "plasma-physics",
      "condensed-matter",
      "biophysics",
      "mathematical-physics",
    ],
  },
  {
    title: "Science & AI",
    ids: [
      "machine-learning",
      "microgpt",
      "genomics",
      "cosmology",
      "astrophysics",
      "complex-systems",
      "chaos-theory",
    ],
  },
];

export default function HomePage() {
  const courseMap = Object.fromEntries(courses.map((c) => [c.id, c]));

  return (
    <div className="h-screen bg-background overflow-y-auto">
      <header className="bg-background sticky top-0 z-10">
        <div className="max-w-3xl mx-auto px-6 py-6 flex items-center justify-between">
          <Image src={logo} alt="Hypercode" width={200} height={44} priority />
          <ThemeToggle />
        </div>
      </header>

      <main className="max-w-3xl mx-auto px-6 pt-20 pb-32">
        <h1 className="text-4xl sm:text-5xl font-bold tracking-tight text-foreground font-display mb-3">
          Learn by doing.
        </h1>
        <p className="text-muted-foreground text-lg mb-16 max-w-lg">
          Interactive courses that run in your browser.
          <br />
          Write code, get instant feedback.
        </p>

        {/* Ordered curriculums */}
        {orderedCurriculums.map((curriculum) => {
          const curriculumCourses = curriculum.ids.map((id) => courseMap[id]).filter(Boolean);
          if (curriculumCourses.length === 0) return null;
          return (
            <section key={curriculum.title} className="mb-14">
              <div className="mb-4 flex items-center gap-3">
                <h2 className="text-xs font-semibold uppercase tracking-widest text-muted-foreground">
                  {curriculum.title} — Curriculum
                </h2>
                <div className="flex-1 h-px bg-border" />
              </div>

              <p className="text-sm text-muted-foreground mb-5 max-w-xl">
                {curriculum.subtitle}
              </p>

              <div className="flex flex-col -space-y-px">
                {curriculumCourses.map((course, i) => (
                  <Link
                    key={course.id}
                    href={`/${course.id}`}
                    className="group flex items-center gap-4 border border-border px-5 py-4 transition-colors hover:bg-accent/50"
                  >
                    <div className="shrink-0 w-7 h-7 rounded-full bg-muted flex items-center justify-center text-xs font-bold text-muted-foreground font-mono">
                      {i + 1}
                    </div>
                    <div className="shrink-0">
                      {courseLogos[course.id] ?? (
                        <div className="w-8 h-8 rounded bg-muted flex items-center justify-center text-xs text-muted-foreground font-mono font-bold">
                          {course.language.slice(0, 2).toUpperCase()}
                        </div>
                      )}
                    </div>
                    <div className="min-w-0 flex-1">
                      <div className="text-sm font-semibold text-foreground font-display">{course.title}</div>
                      <div className="text-xs text-muted-foreground mt-0.5 line-clamp-1">{course.description}</div>
                    </div>
                    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" className="shrink-0 text-muted-foreground/40 transition-all group-hover:text-foreground group-hover:translate-x-0.5">
                      <path d="M6 3.5L10.5 8L6 12.5" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/>
                    </svg>
                  </Link>
                ))}
              </div>
            </section>
          );
        })}

        {/* Unordered tracks */}
        {tracks.map((track) => {
          const trackCourses = track.ids.map((id) => courseMap[id]).filter(Boolean);
          if (trackCourses.length === 0) return null;
          return (
            <section key={track.title} className="mb-10">
              <div className="mb-3 flex items-center gap-3">
                <h2 className="text-xs font-semibold uppercase tracking-widest text-muted-foreground">
                  {track.title}
                </h2>
                <div className="flex-1 h-px bg-border" />
              </div>
              <div className="flex flex-col -space-y-px">
                {trackCourses.map((course) => (
                  <Link
                    key={course.id}
                    href={`/${course.id}`}
                    className="group flex items-center gap-5 border border-border px-6 py-4 transition-colors hover:bg-accent/50"
                  >
                    <div className="shrink-0">
                      {courseLogos[course.id] ?? (
                        <div className="w-8 h-8 rounded bg-muted flex items-center justify-center text-xs text-muted-foreground font-mono font-bold">
                          {course.language.slice(0, 2).toUpperCase()}
                        </div>
                      )}
                    </div>
                    <div className="min-w-0 flex-1">
                      <h3 className="text-base font-semibold text-foreground font-display mb-0.5">
                        {course.title}
                      </h3>
                      <p className="text-sm text-muted-foreground leading-relaxed line-clamp-1">
                        {course.description}
                      </p>
                    </div>
                    <svg
                      width="16"
                      height="16"
                      viewBox="0 0 16 16"
                      fill="none"
                      className="shrink-0 text-muted-foreground/40 transition-all group-hover:text-foreground group-hover:translate-x-0.5"
                    >
                      <path
                        d="M6 3.5L10.5 8L6 12.5"
                        stroke="currentColor"
                        strokeWidth="1.5"
                        strokeLinecap="round"
                        strokeLinejoin="round"
                      />
                    </svg>
                  </Link>
                ))}
              </div>
            </section>
          );
        })}
      </main>
    </div>
  );
}
