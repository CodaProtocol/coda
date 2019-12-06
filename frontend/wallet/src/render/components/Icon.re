type kind =
  | Settings
  | BentArrow
  | ChevronDown
  | EmptyChevronRight
  | Question
  | Warning
  | Info
  | Dots
  | Success
  | Danger
  | BackArrow
  | Locked
  | Unlocked
  | Cross
  | Copy;

[@react.component]
let make = (~kind) =>
  <svg
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="currentColor"
    stroke="currentColor"
    strokeWidth="0"
    xmlns="http://www.w3.org/2000/svg">
    {switch (kind) {
     | Settings =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M18.5828 9.72512L18.2759 8.98211C19.3284 6.55734 19.26 6.48468 19.054 6.27771L17.7088 4.93818L17.5754 4.82268H17.4203C17.337 4.82268 17.0936 4.82268 15.0472 5.76711L14.2938 5.45722C13.3203 3 13.2216 3 12.9323 3H11.0383C10.7534 3 10.6426 3 9.74179 5.46642L8.99194 5.77631C7.61074 5.18119 6.79944 4.87484 6.58075 4.87484H6.40017L4.95548 6.31719C4.73397 6.54255 4.65639 6.61651 5.77023 9.00392L5.46338 9.74482C3 10.7146 3 10.8083 3 11.1132V13.0027C3 13.2999 3 13.4048 5.47241 14.3077L5.77926 15.0472C4.72678 17.472 4.79662 17.5446 5.00118 17.7516L6.34641 19.0932L6.47973 19.2102H6.63704C6.71816 19.2102 6.96012 19.2102 9.00795 18.2636L9.75923 18.5756C10.7327 21.0292 10.8322 21.0292 11.1263 21.0292H13.0169C13.3075 21.0292 13.4105 21.0292 14.3155 18.5643L15.0689 18.2544C16.4501 18.8495 17.2578 19.1558 17.4758 19.1558H17.6564L19.1152 17.7008C19.3225 17.4881 19.3952 17.4142 18.2884 15.0359L18.5939 14.295C21.0585 13.3146 21.0585 13.2118 21.0585 12.916V11.0266C21.0585 10.7294 21.0585 10.6244 18.5825 9.72505L18.5828 9.72512ZM12.0295 15.168C11.1908 15.1701 10.3859 14.8391 9.79192 14.2482C9.19726 13.6573 8.86291 12.8544 8.86221 12.0171C8.8608 11.1804 9.19304 10.3768 9.78559 9.78375C10.3781 9.19145 11.1823 8.85834 12.021 8.85764C12.8597 8.85764 13.6639 9.19007 14.2572 9.78235C14.8504 10.3746 15.1827 11.1775 15.1827 12.0149C15.1813 12.8502 14.8483 13.6502 14.2579 14.2412C13.6668 14.8321 12.8661 15.1652 12.0295 15.168L12.0295 15.168Z"
       />
     | ChevronDown =>
       <path
         d="M10.8339 15.5531L5.48633 10.605C4.44743 9.64366 5.18322 8 6.65244 8L17.3476 8C18.8168 8 19.5526 9.64366 18.5137 10.605L13.1661 15.5531C12.5221 16.149 11.4779 16.149 10.8339 15.5531Z"
       />
     | EmptyChevronRight =>
       <path
         d="M13.6721 6.93658L8.73555 2L6 4.73709L13.1493 11.8864L9.58124 15.4545C7.61876 17.417 6.01283 19.0345 6.01315 19.0489C6.01315 19.0636 6.62606 19.6881 7.37474 20.4368L8.73671 21.7987L13.6721 16.8618C16.3872 14.1467 18.6087 11.9136 18.6087 11.8989C18.6087 11.8842 16.3872 9.65161 13.6721 6.93658Z"
       />
     | BentArrow =>
       <path
         d="M7.3273 6V14.2316H13.0437V11.5L18.5 14.8353L13.0437 18V15.4391H6V6H7.3273Z"
         strokeWidth="0.5"
       />
     | Question =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M12 21C16.9706 21 21 16.9706 21 12C21 7.02944 16.9706 3 12 3C7.02944 3 3 7.02944 3 12C3 16.9706 7.02944 21 12 21ZM10.9299 11.8425V14.4011H12.6034V12.9207C12.9682 12.8563 13.3222 12.7437 13.6655 12.5828C14.0088 12.4111 14.3146 12.1912 14.5828 11.923C14.8617 11.6548 15.0816 11.3329 15.2425 10.9575C15.4142 10.5713 15.5 10.1368 15.5 9.65402C15.5 9.18199 15.4195 8.75287 15.2586 8.36667C15.0977 7.96973 14.8671 7.63716 14.5667 7.36897C14.2663 7.09004 13.9015 6.87548 13.4724 6.72529C13.0433 6.5751 12.5659 6.5 12.0402 6.5C11.5253 6.5 11.0693 6.5751 10.6724 6.72529C10.2755 6.86475 9.93218 7.05785 9.64253 7.3046C9.35287 7.54061 9.11149 7.81418 8.91839 8.12529C8.73602 8.4364 8.59655 8.75824 8.5 9.0908L10.0287 9.7023C10.1467 9.21954 10.3667 8.82261 10.6885 8.51149C11.0103 8.20038 11.4395 8.04483 11.9759 8.04483C12.4801 8.04483 12.8824 8.18429 13.1828 8.46322C13.4831 8.73142 13.6333 9.11226 13.6333 9.60575V9.83103C13.6333 10.4747 13.3973 10.9628 12.9253 11.2954C12.464 11.6172 11.7989 11.7996 10.9299 11.8425ZM10.8816 17.8287C11.0747 18.0218 11.3751 18.1184 11.7828 18.1184C12.1904 18.1184 12.4854 18.0218 12.6678 17.8287C12.8609 17.6249 12.9575 17.3674 12.9575 17.0563V16.7828C12.9575 16.4716 12.8609 16.2195 12.6678 16.0264C12.4854 15.8226 12.1904 15.7207 11.7828 15.7207C11.3751 15.7207 11.0747 15.8226 10.8816 16.0264C10.6992 16.2195 10.608 16.4716 10.608 16.7828V17.0563C10.608 17.3674 10.6992 17.6249 10.8816 17.8287Z"
       />
     | Info =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M12 21C16.9706 21 21 16.9706 21 12C21 7.02944 16.9706 3 12 3C7.02944 3 3 7.02944 3 12C3 16.9706 7.02944 21 12 21ZM10.9429 8.384C11.1669 8.59733 11.5189 8.704 11.9989 8.704C12.4789 8.704 12.8256 8.59733 13.0389 8.384C13.2629 8.16 13.3749 7.87733 13.3749 7.536V7.184C13.3749 6.84267 13.2629 6.56533 13.0389 6.352C12.8256 6.128 12.4789 6.016 11.9989 6.016C11.5189 6.016 11.1669 6.128 10.9429 6.352C10.7296 6.56533 10.6229 6.84267 10.6229 7.184V7.536C10.6229 7.87733 10.7296 8.16 10.9429 8.384ZM13.1829 18V10.6H10.8149V18H13.1829Z"
       />
     | Success =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M10.2063 16.4811L10.181 16.4586L10.1578 16.4811L6.22874 12.5513L7.62585 11.1549L10.181 13.7151L16.374 7.51912L17.7704 8.91692L10.2063 16.4811ZM11.9986 3C7.03092 3 3 7.0311 3 12.0007C3 16.9703 7.0311 21 11.9986 21C16.9682 21 21 16.9703 21 12.0007C21 7.0311 16.9684 3 11.9986 3Z"
       />
     | Danger =>
       <path
         d="M14.919 3.51301L9.1226 3.51301C8.7219 3.51301 8.34781 3.67318 8.05408 3.96692L3.96674 8.05426C3.68604 8.33496 3.51283 8.72208 3.51283 9.12278L3.51283 14.9192C3.51283 15.3199 3.673 15.694 3.96674 15.9877L8.06704 20.088C8.34774 20.3687 8.73486 20.5419 9.13555 20.5419L14.919 20.529C15.3197 20.529 15.6938 20.3688 15.9876 20.0751L20.0879 15.9748C20.3685 15.6941 20.5418 15.307 20.5418 14.9063L20.5288 9.12278C20.5288 8.72208 20.3686 8.348 20.0749 8.05426L15.9876 3.96692C15.6938 3.67318 15.3197 3.51301 14.919 3.51301V3.51301ZM15.8007 9.62987L13.3965 12.034L15.8007 14.4382L14.4379 15.801L12.0338 13.3968L9.62961 15.801L8.25376 14.4251L10.6579 12.0209L8.25376 9.61678L9.62961 8.24093L12.0338 10.6451L14.4379 8.24093L15.8138 9.61678L15.8007 9.62987Z"
       />
     | Warning =>
       <path
         d="M5.34655 19H19.6599C20.6608 19 21.3052 17.9697 20.8527 17.0999L13.696 4.72256C13.2025 3.75915 11.7904 3.75915 11.2967 4.72256L4.14004 17.0999C3.70131 17.9696 4.34569 19 5.34654 19H5.34655ZM13.6276 16.1231C13.6276 16.7654 13.1478 17.2605 12.4623 17.2605C11.7768 17.2605 11.2969 16.7654 11.2969 16.1231V16.0963C11.2969 15.4541 11.7768 14.959 12.4623 14.959C13.1478 14.959 13.6276 15.4541 13.6276 16.0963V16.1231ZM11.8316 7.76012H13.1752C13.5454 7.76012 13.7648 8.06788 13.7237 8.45592L13.1341 13.4871C13.093 13.8484 12.8462 14.0759 12.5034 14.0759C12.1607 14.0759 11.9139 13.8484 11.8727 13.4871L11.2832 8.45592C11.2421 8.06788 11.4614 7.76012 11.8316 7.76012H11.8316Z"
       />
     | BackArrow =>
       <path
         d="M10.9956 20L3 12L10.9956 4L13.3263 6.332L9.29034 10.3509H21V13.6491L9.29034 13.6491L13.3263 17.668L10.9956 20Z"
       />
     | Dots =>
       <>
         <rect x="16" y="10" width="4" height="4" rx="2" />
         <rect x="10" y="10" width="4" height="4" rx="2" />
         <rect x="4" y="10" width="4" height="4" rx="2" />
       </>
     | Locked =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M13.46 4C15.52 4 17.18 5.68 17.18 7.72V10.6H18.54V20.16H6V10.6H7.34V7.72C7.34 5.66 9.02 4 11.06 4H13.46ZM11.06 5.8C10 5.8 9.14 6.66 9.14 7.72V10.6H15.38V7.72C15.38 6.66 14.52 5.8 13.46 5.8H11.06ZM11.16 17.6H13.38L13.02 15.52C13.26 15.3 13.4 15 13.4 14.66C13.4 14.04 12.9 13.52 12.26 13.52C11.62 13.52 11.12 14.02 11.12 14.66C11.12 14.98 11.26 15.28 11.48 15.5L11.16 17.6Z"
       />
     | Unlocked =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M17.42 11.2809H18.52H18.54V20.8409H6V11.2809H15.1L15.22 11.2009C15.64 10.9009 15.92 10.4609 16 9.96093C16.08 9.46093 15.96 8.96093 15.66 8.54093L14.26 6.60093C13.64 5.74093 12.44 5.54093 11.58 6.16093L9.02 8.00093L7.96 6.54093L10.52 4.70093C12.18 3.50093 14.5 3.88093 15.7 5.54093L17.1 7.48093C17.68 8.28093 17.92 9.26093 17.76 10.2409C17.7 10.6009 17.58 10.9609 17.42 11.2809ZM13.36 18.2807H11.14L11.46 16.1807C11.24 15.9607 11.1 15.6607 11.1 15.3407C11.1 14.7007 11.6 14.2007 12.24 14.2007C12.88 14.2007 13.38 14.7207 13.38 15.3407C13.38 15.6807 13.24 15.9807 13 16.2007L13.36 18.2807Z"
         fill="#479056"
       />
     | Cross =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M6.55558 5L5.00002 6.55546L10.4449 12L5 17.4446L6.55556 19.0001L12.0005 13.5555L17.4445 18.9992L19 17.4437L13.556 12L19 6.55636L17.4444 5.00089L12.0005 10.4446L6.55558 5Z"
       />
     | Copy =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M4.13793 3.42857V4.02685V16.5907H8.7516V15.3941H5.29323V4.62513H13.7654V6.21275H14.9207V3.42857H4.13793ZM9.90689 7.4093V8.00758V20.5714H20.6897V7.4093H9.90689ZM19.5344 8.60586H11.0622V19.3749H19.5344V8.60586Z"
       />
     }}
  </svg>;
