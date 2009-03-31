<html>
<head>


<link rel="stylesheet" href="/demo.css"/>
<title>extracts</title>
<style type="text/css">
  div { padding: 5px 5px 5px 5px; }
</style>
</head>

<body>

  <ul>
    <li>
      <input type="checkbox" id="c1"/>
      checked: <span>{! $B("c1") !}</span>
    </li>
    
    <li>
      <input type="radio" id="c2a" name="r2" value="first"/>
      <input type="radio" id="c2b" name="r2" value="second"/>
      
      <ul>
        <li>
          first selected: <span>{! $B("c2a") !}</span>
        </li>
        
        <li>
          which selected: <span>{! $B("c2b") !}</span>
        </li>
      </ul>

    </li>
    
    <li>select-one:
      <select id="c4">
        <option value="av">a</option>
        <option value="bv">b</option>
        <option value="cv">c</option>
      </select>
      
      which selected: <span>{! $B("c4") !}</span>
      
    </li>
    
    <li>select-multiple:
      <select id="c5" multiple>
        <option value="av">a</option>
        <option value="bv">b</option>
        <option value="cv">c</option>
      </select>
      
      which selected: <span>{! "[" + $B("c5") + "]" !}</span>
      
    </li>

    <li>text:
      <input type="text" id="c6"> entered: <span>{! $B("c6") !}</span>
    </li>

    <li>textarea:
      <textarea id="c7">def txt</textarea> entered: <span>{! $B("c7") !}</span>
    </li>

    <li>hidden:
      <input type="hidden" id="c8" value="hidden text"/>
      should be 'hidden text': <span>{! $B("c8") !}</span>
    </li>

    <li>password:
      <input type="password" id="c9"> entered: <span>{! $B("c9") !}</span>
    </li>

      
  </ul>

</body>
</html>
