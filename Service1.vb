'Imports MSSQLLibrary
'Imports ELMDataSvr.DataObjects
'Imports System.Windows.Forms
Imports MySQLLibrary
Imports System.IO
Imports System.Configuration
Imports System.Net

Public Class Service1
    Public timer1 As System.Timers.Timer
    Public Shared serverName As String = ""
    Public Shared appName As String = ""
    Private oTransactionService As New TransactionsWebService.TransactionsService
    Private oDataMoverHQ As datamoverhq
    Private oTranscopes As Transcopes
    Private oTranscopeInstruments As TranscopeInstruments
    Private PurgingVolume As Integer
    Private oPollDataMgr As PollDataMgr
    Private oPollDataHQ As PollDataHQ
    Private oPollDataOthers As polldataothers
    Private oFraudUpdateHQ As fraudupdatehq
    Private oPOCFraud As pocfraudupdatehq
    Private dayOfWeek As Integer
    Private transmgr As TransactionMgr
    Private branchCode As String
    Private computerName As String = String.Empty
    Private Const IPsepa As String = "."
    Private Const sepa As String = "¬"
    Private cr As System.Net.NetworkCredential
    Private pr As System.Net.WebProxy
    ' Me.Url = "http://" & Service1.serverName & "/" & Service1.appName & "/WebServices/TransactionsService.asmx"


    Private Sub SetProxy()
1:      If My.Settings.Proxy_Active Then
2:          cr = New System.Net.NetworkCredential(My.Settings.Proxy_Username, My.Settings.Proxy_Password, My.Settings.Proxy_Domain)
3:          pr = New WebProxy(My.Settings.Proxy_Server, Integer.Parse(My.Settings.Proxy_Port))
4:          pr.Credentials = cr
5:          oTransactionService.Proxy = pr
        End If
       
        'Me.LogError("UsingTheFollingServerAsHQ", oTransactionService.Url, Date.Now)
55:     oTransactionService.Url = "http://" + Service1.serverName + "/" + Service1.appName + "/WebServices/TransactionsService.asmx"
        'Me.LogError("CurrentUrl", oTransactionService.Url, Date.Now)
550:    oTransactionService.Credentials = System.Net.CredentialCache.DefaultCredentials
551:    'Me.LogError("Proxy", "Proxy is " + My.Settings.Proxy_Active, Date.Now)


    End Sub

    Private Sub SyncDBwithHQ()
        Try
6:          oTransactionService = New TransactionsWebService.TransactionsService
7:          SetProxy()
            '   branchCode
            '   gejuehgeyr87784849494844443
8:          Dim dReply As String = String.Empty
            Try
9:              dReply = oTransactionService.SyncDB(branchCode, "gejuehgeyr87784849494844443")
            Catch ex As Exception
10:             Me.LogError("SyncDBwithHQ", ex.Message, Date.Now)
            End Try

11:         If dReply = String.Empty Then
                'Me.LogError("SyncDBwithHQ", "Sync Completed! ", Date.Now)   '("Sync Completed! ", appName, MessageBoxButtons.OK, MessageBoxIcon.Information)            
            Else
12:             Dim reply As String() = dReply.Split(sepa)
13:             Dim counter As Integer = 0

14:             While counter <= reply.Length - 8
15:                 Dim oTranscope As New Transcopes(ConnectionClass.strMgrCnn)
16:                 oTranscope.Where.XNo.Operator = WhereParameter.Operand.Equal
17:                 oTranscope.Where.XNo.Value = reply(counter)
18:                 oTranscope.Query.Load()
19:
20:                 If oTranscope.EOF Then
21:                     Dim oTrans As New Transcopes(ConnectionClass.strMgrCnn)
22:                     With oTrans
23:                         .AddNew()
24:                         .XNo = reply(counter)
25:                         .G = Integer.Parse(reply(counter + 1))
26:                         .LastName = reply(counter + 2)
27:                         .OtherNames = reply(counter + 3)
28:                         .PersonID = Long.Parse(reply(counter + 4))
29:                         .TransactionType = reply(counter + 5)
30:                         .XDate = Now ' Date.Parse(reply(counter + 6))
31:                         .NodeID = reply(counter + 7)
32:                         .CaptureDateTime = .XDate
33:
34:                         .Polled = 1
36:                         .PolledToHQ = 1
37:                         .Processing = 1
38:
39:                         .Save()
40:                     End With
41:                 End If
42:
43:                 counter += 8
44:             End While
45:
46:
47:             'Me.LogError("SyncDBwithHQ", "Sync Completed! ", Date.Now)
48:         End If
49:     Catch ex As Exception
50:         Me.LogError("SyncDBwithHQ", ex.Message, Date.Now) ' AppName, MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Function GetMachineNameIP() As String
51:     Dim ip As String = String.Empty
52:     Dim osysparam As New SystemParameters(ConnectionClass.strMgrCnn)
        Try
53:         osysparam.LoadAll()
54:         If (Not osysparam.EOF) Then
5:              If osysparam.s_UseMachineName = Nothing Or osysparam.s_UseMachineName = "" Or osysparam.s_UseMachineName = "1" Then
56:                 osysparam.s_UseMachineName = "1"
57:                 ip = My.Computer.Name
58:                 osysparam.Save()
59:             Else
60:                 Dim host As IPHostEntry = Dns.GetHostEntry(System.Net.Dns.GetHostName().ToString())
61:                 Dim ipaddr As IPAddress() = host.AddressList
62:                 For Each addr As IPAddress In ipaddr
63:                     If addr.ToString().Split(IPsepa).Length = 4 And addr.AddressFamily = Sockets.AddressFamily.InterNetwork Then
64:                         ip = addr.ToString()
65:                         Exit For
66:                     End If
67:                 Next addr
68:             End If
            End If
69:     Catch ex As Exception

        Finally
70:         If ip = String.Empty Then
71:             Throw New Exception("Error : Machine name/IP was not retrieved")
72:         End If
73:     End Try
74:     Return ip
    End Function

    Private Sub LoadSettings()
75:     'Dim FileName As String = My.Application.Info.DirectoryPath & "\Datamover.dmo"

76:     Dim ConnectionString As String
        '        = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" & _
        '             FileName & "; Jet OLEDB:Database Password=bankanynet55;"
        '77:     Dim connection As Data.OleDb.OleDbConnection
        '78:     Dim command As Data.OleDb.OleDbCommand
79:     Dim oreg As RegistrationCentres
80:     Try
            '81:         connection = New Data.OleDb.OleDbConnection
            '82:         connection.ConnectionString = ConnectionString
            '83:         connection.Open()
            '84:         command = New Data.OleDb.OleDbCommand
            '85:         command.Connection = connection
            '86:         command.CommandType = CommandType.Text
            '87:         command.CommandText = "SELECT UserName,UserPassword,ServerName FROM Mgr"
            '88:         Dim reader As Data.OleDb.OleDbDataReader = command.ExecuteReader
            '89:         reader.Read()
90:         ConnectionString = "Database=bankeyeizumgr;Data Source=" & ConfigurationManager.AppSettings("DESN") & _
                ";User Id=" & ConfigurationManager.AppSettings("DEUI") & ";Password=" & ConfigurationManager.AppSettings("DEUP") & ";"
91:         'ConnectionString = "Data Source=" & reader.GetString(2) & "\SQLEXPRESS;Initial Catalog=BankEyeIzUMGR;Integrated Security=True"
92:         ConnectionClass.strMgrCnn = ConnectionString
93:         'reader.Close()

94:         oreg = New RegistrationCentres(ConnectionClass.strMgrCnn)
95:         oreg.LoadAll()
96:         If Not oreg.EOF Then branchCode = oreg.RegistrationCentreID
            'If (computerName = String.Empty Or computerName = Nothing Or computerName = "") Then
97:         computerName = GetMachineNameIP()
            'End If


        Catch ex As Exception
            'MessageBox.Show(ex.Message, "", MessageBoxButtons.OK, MessageBoxIcon.Information)
98:         Me.LogError("LoadSettings", ex.Message, Date.Now)
        Finally
99:         'If Not (Command() Is Nothing) Then Command.Dispose()
100:        'Command = Nothing
101:        'If Not (connection Is Nothing) Then connection.Close()
102:        'connection = Nothing
103:        oreg = Nothing
        End Try

    End Sub

    Protected Overrides Sub OnStart(ByVal args() As String)
        ' Add code here to start your service. This method should set things
        ' in motion so your service can do its work.
103:    timer1 = New System.Timers.Timer(45000)
107:    AddHandler timer1.Elapsed, AddressOf timer1_Elapsed
108:    timer1.Start()
    End Sub

    Protected Overrides Sub OnStop()
        ' Add code here to perform any tear-down necessary to stop your service.
109:    timer1.Stop()
    End Sub

    Private Sub timer1_Elapsed(ByVal pSender As Object, ByVal pArgs As System.Timers.ElapsedEventArgs)
        Try
110:        timer1.Stop()
111:        Me.LoadSettings()
112:        SyncDBwithHQ()
113:        Me.MoveData()
        Catch ex As Exception
        Finally
114:        timer1.Start()
        End Try
    End Sub

    Private Sub GetUsers()
115:    Dim oIZUUser As Users
116:    Try
117:        If (Not oTransactionService Is Nothing) Then oTransactionService.Dispose()
119:        'oPollDataHQ = New PollDataHQ(ConnectionClass.strMgrCnn)
            'oPollDataHQ.LoadAll()
120:        Service1.serverName = Me.oDataMoverHQ.ServerName
121:        Service1.appName = Me.oDataMoverHQ.ApplicationName
122:        oTransactionService = New TransactionsWebService.TransactionsService
123:        SetProxy()
124:        Dim lastid As Long = 0
125:        Dim userStrings() As String
126:        Dim userDates() As Date
127:        Dim userBooleans() As Boolean
            While True
128:            lastid = oTransactionService.GetNextUser(lastid, branchCode, userStrings, userDates, userBooleans, "gejuehgeyr87784849494844443")
129:            If lastid = 0 Then
130:                Exit While
131:            Else
132:                oIZUUser = New Users(ConnectionClass.strMgrCnn)
133:                oIZUUser.Where.UserID.Operator = WhereParameter.Operand.Equal
134:                oIZUUser.Where.UserID.Value = userStrings(0)
135:                oIZUUser.Query.Load()
136:                If oIZUUser.EOF Then
137:                    oIZUUser = New Users(ConnectionClass.strMgrCnn)
139:                    oIZUUser.AddNew()
140:                    oIZUUser.s_UserID = userStrings(0)
141:                End If
142:                oIZUUser.s_User = userStrings(1)
143:                If userBooleans(0) Then oIZUUser.s_Password = userStrings(2)

144:                oIZUUser.s_CreatedBy = userStrings(3)
145:                oIZUUser.s_LastUpdatedBy = userStrings(4)
146:
147:                oIZUUser.LastUpdatedOn = userDates(0)
148:                oIZUUser.LastLogOn = userDates(1)
149:

150:                oIZUUser.AddCountry = IIf(userBooleans(1), 1, 0)
151:                oIZUUser.AddState = IIf(userBooleans(2), 1, 0)
160:                oIZUUser.AddTransactionCategory = IIf(userBooleans(3), 1, 0)
161:                oIZUUser.AddUser = IIf(userBooleans(4), 1, 0)

170:                oIZUUser.BrowseCountry = IIf(userBooleans(5), 1, 0)
171:                oIZUUser.BrowseState = IIf(userBooleans(6), 1, 0)
180:                oIZUUser.BrowseTransaction = IIf(userBooleans(7), 1, 0)
181:                oIZUUser.BrowseTransactionCategory = IIf(userBooleans(8), 1, 0)
182:                oIZUUser.BrowseUser = IIf(userBooleans(9), 1, 0)
183:
184:                oIZUUser.CanConfigureDataMover = IIf(userBooleans(10), 1, 0)
185:                oIZUUser.CanConfigurePollData = IIf(userBooleans(11), 1, 0)
186:                oIZUUser.CanRegister = IIf(userBooleans(12), 1, 0)
187:                oIZUUser.CaptureImage = IIf(userBooleans(13), 1, 0)

188:                oIZUUser.DeleteCountry = IIf(userBooleans(14), 1, 0)
189:                oIZUUser.DeleteState = IIf(userBooleans(15), 1, 0)
190:                oIZUUser.DeleteTransactionCategory = IIf(userBooleans(16), 1, 0)
191:                oIZUUser.DeleteUser = IIf(userBooleans(17), 1, 0)

192:                oIZUUser.EditCountry = IIf(userBooleans(18), 1, 0)
193:                oIZUUser.EditState = IIf(userBooleans(19), 1, 0)
199:                oIZUUser.EditTransactionCategory = IIf(userBooleans(20), 1, 0)
200:                oIZUUser.EditUser = IIf(userBooleans(21), 1, 0)
201:                oIZUUser.ExportTransaction = IIf(userBooleans(22), 1, 0)

202:                oIZUUser.PrintTransaction = IIf(userBooleans(23), 1, 0)

203:                oIZUUser.SetupBranch = IIf(userBooleans(24), 1, 0)
204:                oIZUUser.SetUpSystemParameters = IIf(userBooleans(25), 1, 0)
205:                oIZUUser.SetUpTranscopeParameters = IIf(userBooleans(26), 1, 0)
206:                oIZUUser.SetUpUser = IIf(userBooleans(27), 1, 0)
207:                oIZUUser.SuperUser = IIf(userBooleans(28), 1, 0)
208:                oIZUUser.Suspended = IIf(userBooleans(29), 1, 0)

209:                oIZUUser.Upload = IIf(userBooleans(30), 1, 0)

210:                oIZUUser.ViewReports = IIf(userBooleans(31), 1, 0)
211:                oIZUUser.Save()
                End If
            End While
        Catch ex As Exception
212:        Me.LogError("GetUsers", ex.Message, Date.Now)
        Finally
213:        oIZUUser = Nothing
        End Try
    End Sub


    Private Sub RegisterComputerWithHQ()
        Try
214:        oTransactionService = New TransactionsWebService.TransactionsService
215:        SetProxy()
216:        Dim reply As String = oTransactionService.MGRStatusToHQ(branchCode, computerName, "bankanynet55", "83hgeyd63vgfg737fg37f833eedapqo")
            'MessageBox.Show(reply)

        Catch ex As Exception
217:        Me.LogError("RegisterComputerWithHQ", ex.Message, Date.Now)
        Finally
        End Try
    End Sub

    Private Sub RegisterPOCsWithHQ()
218:    Dim oMgrList As MgrList
        Try
219:        oTransactionService = New TransactionsWebService.TransactionsService
220:        SetProxy()
221:        oMgrList = New MgrList(ConnectionClass.strMgrCnn)
222:        oMgrList.LoadAll()
223:        Dim reply As String = ""
224:        While Not oMgrList.EOF
225:            reply = oTransactionService.POCStatusToHQ(branchCode, oMgrList.ComputerName, _
                                           oMgrList.s_FaceCameraName, oMgrList.FaceCamera, _
                                                oMgrList.s_InstrumentCameraName, oMgrList.InstrumentCamera, _
                                                oMgrList.s_ScannerName, oMgrList.Scanner, _
                                                oMgrList.s_FingerprintScannerName, oMgrList.FingerprintScanner, _
                                                oMgrList.LastCapture, "bankanynet55", "5cfwhf8fbv72w3789hvvvv")
                'MessageBox.Show(reply)
226:            oMgrList.MoveNext()
            End While
        Catch ex As Exception

        Finally
227:        oMgrList = Nothing
        End Try
    End Sub


    Private Sub GetPOCSettingsFromHQ()
        Try
228:        Dim enables(6) As Boolean
229:        Dim durations(6) As Integer
230:        Dim durationTypes(6) As String
231:        Dim NextPollTimes(6) As Date
232:        Dim PollDurations(6) As Integer
233:        Dim purge As Integer
234:        Dim enabled As Boolean
            'Dim oMgrList As New MgrList(ConnectionClass.strMgrCnn)
            'oMgrList.Where.Suspended.Operator = WhereParameter.Operand.Equal
            'oMgrList.Where.Suspended.Value = False
            'oMgrList.Query.Top = 1
            'oMgrList.Query.Load()
            'If oMgrList.EOF Then Throw New Exception("No POCs Found")
235:        Dim reply As String = oTransactionService.PollDataMgrSettings(enabled, purge, _
                                                                     enables, durations, _
                                                                     durationTypes, NextPollTimes, _
                                                                     PollDurations, "hjf75dehbgv975debgde7dhbd", branchCode, _
                                                                     "Manager")
236:        If reply <> "Successfull" Then
                'MessageBox.Show(reply)
237:        Else
238:            oPollDataMgr = New PollDataMgr(ConnectionClass.strMgrCnn)
239:            oPollDataMgr.LoadAll()
240:            oPollDataMgr.Enabled = IIf(enabled, 1, 0)
                'oPollDataMgr.s_ServerName = Me.txtManagerName.Text
                'oPollDataMgr.s_UserName = Me.txtMgrUser.Text
                'oPollDataMgr.s_ServerPassword = Me.txtMgrPassword.Text
241:            oPollDataMgr.PurgingVolume = purge

242:            oPollDataMgr.MondayEnabled = IIf(enables(0), 1, 0)
243:            oPollDataMgr.TuesdayEnabled = IIf(enables(1), 1, 0)
244:            oPollDataMgr.WednesdayEnabled = IIf(enables(2), 1, 0)
245:            oPollDataMgr.ThursdayEnabled = IIf(enables(3), 1, 0)
246:            oPollDataMgr.FridayEnabled = IIf(enables(4), 1, 0)
247:            oPollDataMgr.SaturdayEnabled = IIf(enables(5), 1, 0)
248:            oPollDataMgr.SundayEnabled = IIf(enables(6), 1, 0)

249:            oPollDataMgr.MondayNextPoll = NextPollTimes(0)
250:            oPollDataMgr.TuesdayNextPoll = NextPollTimes(1)
251:            oPollDataMgr.WednesdayNextPoll = NextPollTimes(2)
252:            oPollDataMgr.ThursdayNextPoll = NextPollTimes(3)
253:            oPollDataMgr.FridayNextPoll = NextPollTimes(4)
254:            oPollDataMgr.SaturdayNextPoll = NextPollTimes(5)
255:            oPollDataMgr.SundayNextPoll = NextPollTimes(6)

256:            oPollDataMgr.MondayDuration = durations(0)
257:            oPollDataMgr.TuesdayDuration = durations(1)
258:            oPollDataMgr.WednesdayDuration = durations(2)
259:            oPollDataMgr.ThursdayDuration = durations(3)
260:            oPollDataMgr.FridayDuration = durations(4)
261:            oPollDataMgr.SaturdayDuration = durations(5)
262:            oPollDataMgr.SundayDuration = durations(6)

263:            oPollDataMgr.MondayDurationType = durationTypes(0)
264:            oPollDataMgr.TuesdayDurationType = durationTypes(1)
265:            oPollDataMgr.WednesdayDurationType = durationTypes(2)
266:            oPollDataMgr.ThursdayDurationType = durationTypes(3)
268:            oPollDataMgr.FridayDurationType = durationTypes(4)
269:            oPollDataMgr.SaturdayDurationType = durationTypes(5)
270:            oPollDataMgr.SundayDurationType = durationTypes(6)

271:            oPollDataMgr.MondayPollDuration = PollDurations(0)
272:            oPollDataMgr.TuesdayPollDuration = PollDurations(1)
273:            oPollDataMgr.WednesdayPollDuration = PollDurations(2)
274:            oPollDataMgr.ThursdayPollDuration = PollDurations(3)
275:            oPollDataMgr.FridayPollDuration = PollDurations(4)
276:            oPollDataMgr.SaturdayPollDuration = PollDurations(5)
277:            oPollDataMgr.SundayPollDuration = PollDurations(6)
278:            oPollDataMgr.Save()
279:            Dim settingsFromHQ As Boolean
280:            Dim settingsFromDate As Date
281:            Dim statusToHQ As Boolean
282:            Dim statusToDate As Date
283:            reply = oTransactionService.PollDataHQSettings(enabled, purge, _
                                                         settingsFromHQ, settingsFromDate, _
                                                         statusToHQ, statusToDate, _
                                                                     enables, durations, _
                                                                     durationTypes, NextPollTimes, _
                                                                     PollDurations, "8365gghdidydgd65456778sd7", branchCode, _
                                                                     "Manager")
284:            If reply = "Successfull" Then

285:                oPollDataHQ = New PollDataHQ(ConnectionClass.strMgrCnn)
286:                oPollDataHQ.LoadAll()
287:                oPollDataHQ.Enabled = IIf(enabled, 1, 0)
288:                oPollDataHQ.SettingsFromHQ = IIf(settingsFromHQ, 1, 0)
289:                oPollDataHQ.SettingsFromHQTime = settingsFromDate
290:                oPollDataHQ.StatusToHQ = IIf(statusToHQ, 1, 0)
292:                oPollDataHQ.StatusToHQTime = statusToDate
293:                oPollDataHQ.PurgingVolume = purge

294:                oPollDataHQ.MondayEnabled = IIf(enables(0), 1, 0)
295:                oPollDataHQ.TuesdayEnabled = IIf(enables(1), 1, 0)
296:                oPollDataHQ.WednesdayEnabled = IIf(enables(2), 1, 0)
297:                oPollDataHQ.ThursdayEnabled = IIf(enables(3), 1, 0)
298:                oPollDataHQ.FridayEnabled = IIf(enables(4), 1, 0)
299:                oPollDataHQ.SaturdayEnabled = IIf(enables(5), 1, 0)
300:                oPollDataHQ.SundayEnabled = IIf(enables(6), 1, 0)

301:                oPollDataHQ.MondayNextPoll = NextPollTimes(0)
302:                oPollDataHQ.TuesdayNextPoll = NextPollTimes(1)
303:                oPollDataHQ.WednesdayNextPoll = NextPollTimes(2)
304:                oPollDataHQ.ThursdayNextPoll = NextPollTimes(3)
305:                oPollDataHQ.FridayNextPoll = NextPollTimes(4)
306:                oPollDataHQ.SaturdayNextPoll = NextPollTimes(5)
307:                oPollDataHQ.SundayNextPoll = NextPollTimes(6)


308:                oPollDataHQ.MondayDuration = durations(0)
309:                oPollDataHQ.TuesdayDuration = durations(1)
310:                oPollDataHQ.WednesdayDuration = durations(2)
311:                oPollDataHQ.ThursdayDuration = durations(3)
312:                oPollDataHQ.FridayDuration = durations(4)
313:                oPollDataHQ.SaturdayDuration = durations(5)
314:                oPollDataHQ.SundayDuration = durations(6)

315:                oPollDataHQ.MondayDurationType = durationTypes(0)
316:                oPollDataHQ.TuesdayDurationType = durationTypes(1)
317:                oPollDataHQ.WednesdayDurationType = durationTypes(2)
318:                oPollDataHQ.ThursdayDurationType = durationTypes(3)
319:                oPollDataHQ.FridayDurationType = durationTypes(4)
320:                oPollDataHQ.SaturdayDurationType = durationTypes(5)
321:                oPollDataHQ.SundayDurationType = durationTypes(6)

322:                oPollDataHQ.MondayPollDuration = PollDurations(0)
323:                oPollDataHQ.TuesdayPollDuration = PollDurations(1)
324:                oPollDataHQ.WednesdayPollDuration = PollDurations(2)
325:                oPollDataHQ.ThursdayPollDuration = PollDurations(3)
326:                oPollDataHQ.FridayPollDuration = PollDurations(4)
327:                oPollDataHQ.SaturdayPollDuration = PollDurations(5)
328:                oPollDataHQ.SundayPollDuration = PollDurations(6)
329:                oPollDataHQ.Save()

330:                reply = oTransactionService.PollDataOthersSettings(enabled, purge, _
                                                                     enables, durations, _
                                                                     durationTypes, NextPollTimes, _
                                                                     PollDurations, "j7h5fv4g8hn5g45v879nb6", branchCode, _
                                                                     "Manager")
331:                If reply <> "Successfull" Then
                        'MessageBox.Show(reply)
                    Else
332:                    oPollDataOthers = New polldataothers(ConnectionClass.strMgrCnn)
333:                    oPollDataOthers.LoadAll()
334:                    oPollDataOthers.Enabled = IIf(enabled, 1, 0)
335:                    oPollDataOthers.MondayEnabled = IIf(enables(0), 1, 0)
336:                    oPollDataOthers.TuesdayEnabled = IIf(enables(1), 1, 0)
337:                    oPollDataOthers.WednesdayEnabled = IIf(enables(2), 1, 0)
338:                    oPollDataOthers.ThursdayEnabled = IIf(enables(3), 1, 0)
339:                    oPollDataOthers.FridayEnabled = IIf(enables(4), 1, 0)
340:                    oPollDataOthers.SaturdayEnabled = IIf(enables(5), 1, 0)
341:                    oPollDataOthers.SundayEnabled = IIf(enables(6), 1, 0)
342:
345:                    oPollDataOthers.MondayNextPoll = NextPollTimes(0)
346:                    oPollDataOthers.TuesdayNextPoll = NextPollTimes(1)
347:                    oPollDataOthers.WednesdayNextPoll = NextPollTimes(2)
349:                    oPollDataOthers.ThursdayNextPoll = NextPollTimes(3)
3499:                   oPollDataOthers.FridayNextPoll = NextPollTimes(4)
351:                    oPollDataOthers.SaturdayNextPoll = NextPollTimes(5)
352:                    oPollDataOthers.SundayNextPoll = NextPollTimes(6)
353:
354:
3455:                   oPollDataOthers.MondayDuration = durations(0)
356:                    oPollDataOthers.TuesdayDuration = durations(1)
357:                    oPollDataOthers.WednesdayDuration = durations(2)
358:                    oPollDataOthers.ThursdayDuration = durations(3)
359:                    oPollDataOthers.FridayDuration = durations(4)
360:                    oPollDataOthers.SaturdayDuration = durations(5)
361:                    oPollDataOthers.SundayDuration = durations(6)
362:
363:                    oPollDataOthers.MondayDurationType = durationTypes(0)
367:                    oPollDataOthers.TuesdayDurationType = durationTypes(1)
368:                    oPollDataOthers.WednesdayDurationType = durationTypes(2)
369:                    oPollDataOthers.ThursdayDurationType = durationTypes(3)
370:                    oPollDataOthers.FridayDurationType = durationTypes(4)
371:                    oPollDataOthers.SaturdayDurationType = durationTypes(5)
372:                    oPollDataOthers.SundayDurationType = durationTypes(6)
373:
374:                    oPollDataOthers.MondayPollDuration = PollDurations(0)
375:                    oPollDataOthers.TuesdayPollDuration = PollDurations(1)
376:
379:                    oPollDataOthers.WednesdayPollDuration = PollDurations(2)
380:
381:
382:
383:                    oPollDataOthers.ThursdayPollDuration = PollDurations(3)
384:                    oPollDataOthers.FridayPollDuration = PollDurations(4)
385:                    oPollDataOthers.SaturdayPollDuration = PollDurations(5)
386:                    oPollDataOthers.SundayPollDuration = PollDurations(6)
387:                    oPollDataOthers.Save()
388:                    'MessageBox.Show("Successfully Loaded Manager Others Settings From HQ")

389:                    oTransactionService.Dispose()
390:                    oPOCFraud = New pocfraudupdatehq(ConnectionClass.strMgrCnn)
391:                    oPOCFraud.LoadAll()

392:                    Service1.serverName = oFraudUpdateHQ.ServerName
393:                    Service1.appName = oFraudUpdateHQ.ApplicationName
394:                    oTransactionService = New TransactionsWebService.TransactionsService
395:                    SetProxy()
396:                    reply = oTransactionService.POCFraudUpdateHQSettings(enabled, purge, _
                                                                 enables, durations, _
                                                                             durationTypes, NextPollTimes, _
                                                                             PollDurations, "yyr12576cnd9yt7eeeu77aa", _
                                                                             branchCode, "Manager")
397:                    If reply <> "Successfull" Then
                            'MessageBox.Show(reply)
                        Else
398:                        oPOCFraud.Enabled = IIf(enabled, 1, 0)
                            'oPOCFraud.s_ServerName = Me.txtManagerName.Text
                            'oPOCFraud.s_UserName = Me.txtMgrUser.Text
                            'oPOCFraud.s_ServerPassword = Me.txtMgrPassword.Text
399:                        oPOCFraud.PurgingVolume = purge

400:                        oPOCFraud.MondayEnabled = IIf(enables(0), 1, 0)
401:                        oPOCFraud.TuesdayEnabled = IIf(enables(1), 1, 0)
402:                        oPOCFraud.WednesdayEnabled = IIf(enables(2), 1, 0)
403:                        oPOCFraud.ThursdayEnabled = IIf(enables(3), 1, 0)
404:                        oPOCFraud.FridayEnabled = IIf(enables(4), 1, 0)
405:                        oPOCFraud.SaturdayEnabled = IIf(enables(5), 1, 0)
406:                        oPOCFraud.SundayEnabled = IIf(enables(6), 1, 0)

407:                        oPOCFraud.MondayNextPoll = NextPollTimes(0)
408:                        oPOCFraud.TuesdayNextPoll = NextPollTimes(1)
409:                        oPOCFraud.WednesdayNextPoll = NextPollTimes(2)
410:                        oPOCFraud.ThursdayNextPoll = NextPollTimes(3)
411:                        oPOCFraud.FridayNextPoll = NextPollTimes(4)
412:                        oPOCFraud.SaturdayNextPoll = NextPollTimes(5)
413:                        oPOCFraud.SundayNextPoll = NextPollTimes(6)

414:                        oPOCFraud.MondayDuration = durations(0)
415:                        oPOCFraud.TuesdayDuration = durations(1)
416:                        oPOCFraud.WednesdayDuration = durations(2)
417:                        oPOCFraud.ThursdayDuration = durations(3)
418:                        oPOCFraud.FridayDuration = durations(4)
419:                        oPOCFraud.SaturdayDuration = durations(5)
420:                        oPOCFraud.SundayDuration = durations(6)

421:                        oPOCFraud.MondayDurationType = durationTypes(0)
422:                        oPOCFraud.TuesdayDurationType = durationTypes(1)
423:                        oPOCFraud.WednesdayDurationType = durationTypes(2)
424:                        oPOCFraud.ThursdayDurationType = durationTypes(3)
425:                        oPOCFraud.FridayDurationType = durationTypes(4)
426:                        oPOCFraud.SaturdayDurationType = durationTypes(5)
427:                        oPOCFraud.SundayDurationType = durationTypes(6)

428:                        oPOCFraud.MondayPollDuration = PollDurations(0)
429:                        oPOCFraud.TuesdayPollDuration = PollDurations(1)
430:                        oPOCFraud.WednesdayPollDuration = PollDurations(2)
431:                        oPOCFraud.ThursdayPollDuration = PollDurations(3)
432:                        oPOCFraud.FridayPollDuration = PollDurations(4)
433:                        oPOCFraud.SaturdayPollDuration = PollDurations(5)
434:                        oPOCFraud.SundayPollDuration = PollDurations(6)
435:                        oPOCFraud.Save()
436:                        'MessageBox.Show("Successfully Fraud Update Settings From HQ")

                        End If

                    End If
                End If

            End If

437:    Catch ex As Exception

        End Try
    End Sub

    Private Sub GetSettingsFromHQ()
        Try
            'Me.gbHQ.Enabled = False
            'Me.gbFraud.Enabled = False
            'Me.MenuStrip1.Enabled = False

437:        Service1.serverName = oDataMoverHQ.ServerName
438:        Service1.appName = oDataMoverHQ.ApplicationName
439:        Dim enables(6) As Boolean
440:        Dim durations(6) As Integer
441:        Dim durationTypes(6) As String
442:        Dim NextPollTimes(6) As Date
443:        Dim PollDurations(6) As Integer
444:        Dim purge As Integer
445:        Dim enabled As Boolean
446:        Dim settingsFromHQ, statusToHQ As Boolean
447:        Dim settingsFromDate, statusToDate As Date
448:        oTransactionService = New TransactionsWebService.TransactionsService
449:        SetProxy()
450:        Dim reply As String = oTransactionService.DataMoverHQSettings(enabled, purge, _
                                                         settingsFromHQ, settingsFromDate, _
                                                         statusToHQ, statusToDate, _
                                                                     enables, durations, _
                                                                     durationTypes, NextPollTimes, _
                                                                     PollDurations, "8767dhdjd766dhbdgv13265xx", _
                                                                     branchCode, computerName)
451:        If reply <> "Successfull" Then
                'MessageBox.Show(reply)
            Else
452:            oDataMoverHQ.Enabled = IIf(enabled, 1, 0)
453:            oDataMoverHQ.SettingsFromHQ = IIf(settingsFromHQ, 1, 0)
454:            oDataMoverHQ.SettingsFromHQTime = settingsFromDate
456:            oDataMoverHQ.StatusToHQ = IIf(statusToHQ, 1, 0)
457:            oDataMoverHQ.StatusToHQTime = statusToDate
458:            oDataMoverHQ.PurgingVolume = purge

459:            oDataMoverHQ.MondayEnabled = IIf(enables(0), 1, 0)
460:            oDataMoverHQ.TuesdayEnabled = IIf(enables(1), 1, 0)
461:            oDataMoverHQ.WednesdayEnabled = IIf(enables(2), 1, 0)
462:            oDataMoverHQ.ThursdayEnabled = IIf(enables(3), 1, 0)
463:            oDataMoverHQ.FridayEnabled = IIf(enables(4), 1, 0)
464:            oDataMoverHQ.SaturdayEnabled = IIf(enables(5), 1, 0)
465:            oDataMoverHQ.SundayEnabled = IIf(enables(6), 1, 0)
466:
467:            oDataMoverHQ.MondayNextPoll = NextPollTimes(0)
468:            oDataMoverHQ.TuesdayNextPoll = NextPollTimes(1)
469:            oDataMoverHQ.WednesdayNextPoll = NextPollTimes(2)
470:            oDataMoverHQ.ThursdayNextPoll = NextPollTimes(3)
471:            oDataMoverHQ.FridayNextPoll = NextPollTimes(4)
472:            oDataMoverHQ.SaturdayNextPoll = NextPollTimes(5)
473:            oDataMoverHQ.SundayNextPoll = NextPollTimes(6)


474:            oDataMoverHQ.MondayDuration = durations(0)
475:            oDataMoverHQ.TuesdayDuration = durations(1)
476:            oDataMoverHQ.WednesdayDuration = durations(2)
477:            oDataMoverHQ.ThursdayDuration = durations(3)
478:            oDataMoverHQ.FridayDuration = durations(4)
479:            oDataMoverHQ.SaturdayDuration = durations(5)
480:            oDataMoverHQ.SundayDuration = durations(6)
481:
482:            oDataMoverHQ.MondayDurationType = durationTypes(0)
483:            oDataMoverHQ.TuesdayDurationType = durationTypes(1)
484:            oDataMoverHQ.WednesdayDurationType = durationTypes(2)
485:            oDataMoverHQ.ThursdayDurationType = durationTypes(3)
486:            oDataMoverHQ.FridayDurationType = durationTypes(4)
487:            oDataMoverHQ.SaturdayDurationType = durationTypes(5)
488:            oDataMoverHQ.SundayDurationType = durationTypes(6)

489:            oDataMoverHQ.MondayPollDuration = PollDurations(0)
490:            oDataMoverHQ.TuesdayPollDuration = PollDurations(1)
491:            oDataMoverHQ.WednesdayPollDuration = PollDurations(2)
492:            oDataMoverHQ.ThursdayPollDuration = PollDurations(3)
493:            oDataMoverHQ.FridayPollDuration = PollDurations(4)
494:            oDataMoverHQ.SaturdayPollDuration = PollDurations(5)
495:            oDataMoverHQ.SundayPollDuration = PollDurations(6)
                oDataMoverHQ.Save()
                'MessageBox.Show("Successfully Loaded HQ Settings From HQ")
                'Me.LoadHQSettings(False)

496:            oTransactionService.Dispose()
497:            Service1.serverName = oFraudUpdateHQ.ServerName
498:            Service1.appName = oFraudUpdateHQ.ApplicationName
499:            oTransactionService = New TransactionsWebService.TransactionsService
500:            SetProxy()
501:            reply = oTransactionService.FraudUpdateHQSettings(enabled, purge, _
                                                         enables, durations, _
                                                                     durationTypes, NextPollTimes, _
                                                                     PollDurations, "yyr12576cnd9yt7eeeu77aa", _
                                                                     branchCode, computerName)
502:            If reply <> "Successfull" Then
                    'MessageBox.Show(reply)
                Else
503:                oFraudUpdateHQ.Enabled = IIf(enabled, 1, 0)
                    'oFraudUpdateHQ.s_ServerName = Me.txtManagerName.Text
                    'oFraudUpdateHQ.s_UserName = Me.txtMgrUser.Text
                    'oFraudUpdateHQ.s_ServerPassword = Me.txtMgrPassword.Text
504:                oFraudUpdateHQ.PurgingVolume = purge

505:                oFraudUpdateHQ.MondayEnabled = IIf(enables(0), 1, 0)
506:                oFraudUpdateHQ.TuesdayEnabled = IIf(enables(1), 1, 0)
507:                oFraudUpdateHQ.WednesdayEnabled = IIf(enables(2), 1, 0)
508:                oFraudUpdateHQ.ThursdayEnabled = IIf(enables(3), 1, 0)
509:                oFraudUpdateHQ.FridayEnabled = IIf(enables(4), 1, 0)
510:                oFraudUpdateHQ.SaturdayEnabled = IIf(enables(5), 1, 0)
511:                oFraudUpdateHQ.SundayEnabled = IIf(enables(6), 1, 0)
512:
513:                oFraudUpdateHQ.MondayNextPoll = NextPollTimes(0)
514:                oFraudUpdateHQ.TuesdayNextPoll = NextPollTimes(1)
516:                oFraudUpdateHQ.WednesdayNextPoll = NextPollTimes(2)
517:                oFraudUpdateHQ.ThursdayNextPoll = NextPollTimes(3)
518:                oFraudUpdateHQ.FridayNextPoll = NextPollTimes(4)
519:                oFraudUpdateHQ.SaturdayNextPoll = NextPollTimes(5)
520:                oFraudUpdateHQ.SundayNextPoll = NextPollTimes(6)
521:
522:
523:                oFraudUpdateHQ.MondayDuration = durations(0)
524:                oFraudUpdateHQ.TuesdayDuration = durations(1)
525:                oFraudUpdateHQ.WednesdayDuration = durations(2)
526:                oFraudUpdateHQ.ThursdayDuration = durations(3)
527:                oFraudUpdateHQ.FridayDuration = durations(4)
528:                oFraudUpdateHQ.SaturdayDuration = durations(5)
529:                oFraudUpdateHQ.SundayDuration = durations(6)

530:                oFraudUpdateHQ.MondayDurationType = durationTypes(0)
531:                oFraudUpdateHQ.TuesdayDurationType = durationTypes(1)
532:                oFraudUpdateHQ.WednesdayDurationType = durationTypes(2)
533:                oFraudUpdateHQ.ThursdayDurationType = durationTypes(3)
534:                oFraudUpdateHQ.FridayDurationType = durationTypes(4)
535:                oFraudUpdateHQ.SaturdayDurationType = durationTypes(5)
536:                oFraudUpdateHQ.SundayDurationType = durationTypes(6)

537:                oFraudUpdateHQ.MondayPollDuration = PollDurations(0)
538:                oFraudUpdateHQ.TuesdayPollDuration = PollDurations(1)
539:                oFraudUpdateHQ.WednesdayPollDuration = PollDurations(2)
540:                oFraudUpdateHQ.ThursdayPollDuration = PollDurations(3)
541:                oFraudUpdateHQ.FridayPollDuration = PollDurations(4)
542:                oFraudUpdateHQ.SaturdayPollDuration = PollDurations(5)
543:                oFraudUpdateHQ.SundayPollDuration = PollDurations(6)
544:                oFraudUpdateHQ.Save()
                    'MessageBox.Show("Successfully Fraud Update Settings From HQ")
                    'Me.LoadFraudSettings(False)
545:                Me.GetPOCSettingsFromHQ()
546:                Me.GetSystemParametersHQ()
                End If

            End If

        Catch ex As Exception
            'MessageBox.Show(ex.Message)
547:        Me.LogError("GetSettingsFromHQ", ex.Message, Date.Now)
        Finally
            'Me.gbHQ.Enabled = True
            'Me.gbFraud.Enabled = True
            'Me.MenuStrip1.Enabled = True

        End Try
    End Sub

    Private Sub GetSystemParametersHQ()
        Dim oSystemParameters As SystemParameters
        Try
547:        oSystemParameters = New SystemParameters(ConnectionClass.strMgrCnn)
548:        oSystemParameters.LoadAll()
549:        Dim booleans() As Boolean
550:        Dim strings() As String
551:        Dim integers() As Integer
552:        Dim reply As String = oTransactionService.GetSystemparameters(strings, booleans, integers, _
                                              oSystemParameters.Logo, "nbiufb3e73ubu3fg73kbcuwwcw", _
                                                  branchCode, True, computerName)
553:        If reply <> "Successfull" Then Throw New Exception(reply)

554:        oSystemParameters.s_OrganisationID = strings(0)
555:        oSystemParameters.s_OrganisationName = strings(1)
556:        oSystemParameters.s_EnrollmentMode = strings(2)
557:        oSystemParameters.s_TemplateSize = strings(3)
558:        oSystemParameters.s_ReturnedImage = strings(4)
559:        oSystemParameters.s_MatchingMode = strings(5)
560:        oSystemParameters.s_CreatedBy = strings(6)
561:        oSystemParameters.s_LastUpdatedBy = strings(7)
562:        oSystemParameters.s_MatchingFAR = Double.Parse(strings(8))
563:        oSystemParameters.s_GeneralizationFAR = Double.Parse(strings(9))

564:        oSystemParameters.s_EnrollmentMode = strings(10)

567:        oSystemParameters.UseScanner = IIf(booleans(0), 1, 0)
568:        oSystemParameters.SearchForDuplicates = IIf(booleans(1), 1, 0)
569:        oSystemParameters.UseMinimalMinutiaCount = IIf(booleans(2), 1, 0)
570:        oSystemParameters.UseGeneralisationOf = IIf(booleans(3), 1, 0)
571:        oSystemParameters.UseQuality = IIf(booleans(4), 1, 0)
5711:       oSystemParameters.MatchAllDatabase = IIf(booleans(5), 1, 0)
572:        oSystemParameters.ShowAllResults = IIf(booleans(6), 1, 0)
573:        oSystemParameters.UseG = IIf(booleans(7), 1, 0)
574:        oSystemParameters.ThumbsCompulsory = IIf(booleans(8), 1, 0)
575:        oSystemParameters.IndicesCompulsory = IIf(booleans(9), 1, 0)
576:        oSystemParameters.MiddleCompulsory = IIf(booleans(10), 1, 0)
577:        oSystemParameters.RingCompulsory = IIf(booleans(11), 1, 0)
578:        oSystemParameters.LittleCompulsory = IIf(booleans(12), 1, 0)
579:        oSystemParameters.CheckFraudHQ = IIf(booleans(13), 1, 0)
580:        oSystemParameters.CheckFraudManager = IIf(booleans(14), 1, 0)
581:        oSystemParameters.CheckFraudLocal = IIf(booleans(15), 1, 0)
582:        oSystemParameters.AutoCheckFraud = IIf(booleans(16), 1, 0)
583:        oSystemParameters.VerifyHQ = IIf(booleans(17), 1, 0)
584:        oSystemParameters.EnableBankEye = IIf(booleans(18), 1, 0)
585:        oSystemParameters.EnableAccountOpening = IIf(booleans(19), 1, 0)

588:        oSystemParameters.MinimalMinutiaCount = integers(0)
589:        oSystemParameters.GeneralisationTemplates = integers(1)
590:        oSystemParameters.Threshold = integers(2)
591:        oSystemParameters.GeneralizationMaximalRotation = integers(3)
592:        oSystemParameters.MatchAtMost = integers(4)
593:        oSystemParameters.MatchingMaximalRotation = integers(5)
594:        oSystemParameters.PhotoQuality = integers(6)
595:        oSystemParameters.InstrumentQuality = integers(7)
596:        oSystemParameters.ScannerQuality = integers(8)


597:        oSystemParameters.Save()
            'MessageBox.Show("System Parameters Successfully Fetched And Saved")
        Catch ex As Exception
587:        Me.LogError("GetSystemParametersHQ", ex.Message, Date.Now)
        End Try
    End Sub

    Private Sub NextOperation(ByVal duration As Integer, ByVal durationType As String, _
                              ByVal pollPeriod As Integer, _
                              ByRef NextPoll As Date, ByRef PollStop As Date)
588:    While NextPoll <= Date.Now
589:        If durationType = "Minute(s)" Then
600:            NextPoll = Date.Now.AddMinutes(duration)
601:        ElseIf durationType = "Hour(s)" Then
602:            NextPoll = NextPoll.AddHours(duration)
            Else
603:            NextPoll = NextPoll.AddDays(duration)
            End If
        End While
604:    PollStop = Date.Now.AddMinutes(pollPeriod)
    End Sub

    Private Sub MoveData()
        Try
605:        Me.dayOfWeek = Date.Now.DayOfWeek
606:        Dim lastPollTime As Date = Date.Now
607:        Dim nextPollTime As Date = Date.Now
608:        Dim shouldPoll As Boolean = False
609:        Dim duration As Integer = 0
610:        Dim durationType As String = ""
611:        Dim pollPeriod As Integer = 0

612:        Me.transmgr = TransactionMgr.ThreadTransactionMgr
            'Poll To HQ
613:        oDataMoverHQ = New datamoverhq(ConnectionClass.strMgrCnn)
614:        oDataMoverHQ.LoadAll()
615:        oFraudUpdateHQ = New fraudupdatehq(ConnectionClass.strMgrCnn)
616:        oFraudUpdateHQ.LoadAll()

617:        If Not oDataMoverHQ.EOF Then
618:            If oDataMoverHQ.Enabled Then
619:                If dayOfWeek = 1 And oDataMoverHQ.MondayEnabled Then

620:                    If nextPollTime >= oDataMoverHQ.MondayNextPoll Then
621:                        shouldPoll = True
623:                        Me.NextOperation(oDataMoverHQ.MondayDuration, _
                                              oDataMoverHQ.MondayDurationType, _
                                              oDataMoverHQ.MondayPollDuration, oDataMoverHQ.MondayNextPoll, _
                                              oDataMoverHQ.MondayPollStop)

                        End If
624:                    If oDataMoverHQ.MondayPollStop >= nextPollTime Then
625:                        shouldPoll = True
                        End If
626:
627:                ElseIf dayOfWeek = 2 And oDataMoverHQ.TuesdayEnabled Then

628:                    If nextPollTime >= oDataMoverHQ.TuesdayNextPoll Then
629:                        shouldPoll = True
630:                        Me.NextOperation(oDataMoverHQ.TuesdayDuration, _
                                              oDataMoverHQ.TuesdayDurationType, _
                                              oDataMoverHQ.TuesdayPollDuration, oDataMoverHQ.TuesdayNextPoll, _
                                              oDataMoverHQ.TuesdayPollStop)

                        End If
631:                    If oDataMoverHQ.TuesdayPollStop >= nextPollTime Then
632:                        shouldPoll = True
                        End If

633:                ElseIf dayOfWeek = 3 And oDataMoverHQ.WednesdayEnabled Then

634:                    If nextPollTime >= oDataMoverHQ.WednesdayNextPoll Then
635:                        shouldPoll = True
636:                        Me.NextOperation(oDataMoverHQ.WednesdayDuration, _
                                              oDataMoverHQ.WednesdayDurationType, _
                                              oDataMoverHQ.WednesdayPollDuration, oDataMoverHQ.WednesdayNextPoll, _
                                              oDataMoverHQ.WednesdayPollStop)

                        End If
637:                    If oDataMoverHQ.WednesdayPollStop >= nextPollTime Then
638:                        shouldPoll = True
                        End If

639:                ElseIf dayOfWeek = 4 And oDataMoverHQ.ThursdayEnabled Then

640:                    If nextPollTime >= oDataMoverHQ.ThursdayNextPoll Then
641:                        shouldPoll = True
642:                        Me.NextOperation(oDataMoverHQ.ThursdayDuration, _
                                              oDataMoverHQ.ThursdayDurationType, _
                                              oDataMoverHQ.ThursdayPollDuration, oDataMoverHQ.ThursdayNextPoll, _
                                              oDataMoverHQ.ThursdayPollStop)

                        End If
643:                    If oDataMoverHQ.ThursdayPollStop >= nextPollTime Then
644:                        shouldPoll = True
                        End If

645:                ElseIf dayOfWeek = 5 And oDataMoverHQ.FridayEnabled Then

646:                    If nextPollTime >= oDataMoverHQ.FridayNextPoll Then
647:                        shouldPoll = True
648:                        Me.NextOperation(oDataMoverHQ.FridayDuration, _
                                              oDataMoverHQ.FridayDurationType, _
                                              oDataMoverHQ.FridayPollDuration, oDataMoverHQ.FridayNextPoll, _
                                              oDataMoverHQ.FridayPollStop)

                        End If
649:                    If oDataMoverHQ.FridayPollStop >= nextPollTime Then
650:                        shouldPoll = True
                        End If

651:                ElseIf dayOfWeek = 6 And oDataMoverHQ.SaturdayEnabled Then

652:                    If nextPollTime >= oDataMoverHQ.SaturdayNextPoll Then
653:                        shouldPoll = True
654:                        Me.NextOperation(oDataMoverHQ.SaturdayDuration, _
                                              oDataMoverHQ.SaturdayDurationType, _
                                              oDataMoverHQ.SaturdayPollDuration, oDataMoverHQ.SaturdayNextPoll, _
                                              oDataMoverHQ.SaturdayPollStop)

                        End If
655:                    If oDataMoverHQ.SaturdayPollStop >= nextPollTime Then
656:                        shouldPoll = True
                        End If

657:                ElseIf dayOfWeek = 7 And oDataMoverHQ.SundayEnabled Then

658:                    If nextPollTime >= oDataMoverHQ.SundayNextPoll Then
659:                        shouldPoll = True
660:                        Me.NextOperation(oDataMoverHQ.SundayDuration, _
                                              oDataMoverHQ.SundayDurationType, _
                                              oDataMoverHQ.SundayPollDuration, oDataMoverHQ.SundayNextPoll, _
                                              oDataMoverHQ.SundayPollStop)

                        End If
661:                    If oDataMoverHQ.SundayPollStop >= nextPollTime Then
662:                        shouldPoll = True
663:                    End If
664:                End If ' dayOfWeek
665:                If shouldPoll Then
666:                    Service1.serverName = oDataMoverHQ.s_ServerName
667:                    Service1.appName = oDataMoverHQ.s_ApplicationName
668:                    oTransactionService = New TransactionsWebService.TransactionsService
669:                    SetProxy()
670:                    Me.PurgingVolume = oDataMoverHQ.PurgingVolume
671:                    Me.RegisterComputerWithHQ()
672:                    Me.RegisterPOCsWithHQ()
673:                    Me.PollHQTransactions()
674:                    oDataMoverHQ.Save()
675:                    Me.GetUsers()
                    End If

                End If 'PollMgr.Enabled

            End If


676:        shouldPoll = False
677:        If Not oFraudUpdateHQ.EOF Then
678:            If oFraudUpdateHQ.Enabled Then
679:                If dayOfWeek = 1 And oFraudUpdateHQ.MondayEnabled Then

680:                    If nextPollTime >= oFraudUpdateHQ.MondayNextPoll Then
682:                        shouldPoll = True
                            Me.NextOperation(oFraudUpdateHQ.MondayDuration, _
                                              oFraudUpdateHQ.MondayDurationType, _
                                              oFraudUpdateHQ.MondayPollDuration, oFraudUpdateHQ.MondayNextPoll, _
                                              oFraudUpdateHQ.MondayPollStop)

                        End If
683:                    If oFraudUpdateHQ.MondayPollStop >= nextPollTime Then
684:                        shouldPoll = True
                        End If

685:                ElseIf dayOfWeek = 2 And oFraudUpdateHQ.TuesdayEnabled Then

686:                    If nextPollTime >= oFraudUpdateHQ.TuesdayNextPoll Then
687:                        shouldPoll = True
688:                        Me.NextOperation(oFraudUpdateHQ.TuesdayDuration, _
                                              oFraudUpdateHQ.TuesdayDurationType, _
                                              oFraudUpdateHQ.TuesdayPollDuration, oFraudUpdateHQ.TuesdayNextPoll, _
                                              oFraudUpdateHQ.TuesdayPollStop)

689:                    End If
690:                    If oFraudUpdateHQ.TuesdayPollStop >= nextPollTime Then
691:                        shouldPoll = True
                        End If

692:                ElseIf dayOfWeek = 3 And oFraudUpdateHQ.WednesdayEnabled Then

693:                    If nextPollTime >= oFraudUpdateHQ.WednesdayNextPoll Then
694:                        shouldPoll = True
695:                        Me.NextOperation(oFraudUpdateHQ.WednesdayDuration, _
                                              oFraudUpdateHQ.WednesdayDurationType, _
                                              oFraudUpdateHQ.WednesdayPollDuration, oFraudUpdateHQ.WednesdayNextPoll, _
                                              oFraudUpdateHQ.WednesdayPollStop)

                        End If
696:                    If oFraudUpdateHQ.WednesdayPollStop >= nextPollTime Then
697:                        shouldPoll = True
                        End If

698:                ElseIf dayOfWeek = 4 And oFraudUpdateHQ.ThursdayEnabled Then

699:                    If nextPollTime >= oFraudUpdateHQ.ThursdayNextPoll Then
700:                        shouldPoll = True
701:                        Me.NextOperation(oFraudUpdateHQ.ThursdayDuration, _
                                              oFraudUpdateHQ.ThursdayDurationType, _
                                              oFraudUpdateHQ.ThursdayPollDuration, oFraudUpdateHQ.ThursdayNextPoll, _
                                              oFraudUpdateHQ.ThursdayPollStop)

                        End If
702:                    If oFraudUpdateHQ.ThursdayPollStop >= nextPollTime Then
703:                        shouldPoll = True
                        End If

704:                ElseIf dayOfWeek = 5 And oFraudUpdateHQ.FridayEnabled Then

705:                    If nextPollTime >= oFraudUpdateHQ.FridayNextPoll Then
706:                        shouldPoll = True
707:                        Me.NextOperation(oFraudUpdateHQ.FridayDuration, _
                                              oFraudUpdateHQ.FridayDurationType, _
                                              oFraudUpdateHQ.FridayPollDuration, oFraudUpdateHQ.FridayNextPoll, _
                                              oFraudUpdateHQ.FridayPollStop)

                        End If
708:                    If oFraudUpdateHQ.FridayPollStop >= nextPollTime Then
709:                        shouldPoll = True
                        End If

710:                ElseIf dayOfWeek = 6 And oFraudUpdateHQ.SaturdayEnabled Then

711:                    If nextPollTime >= oFraudUpdateHQ.SaturdayNextPoll Then
712:                        shouldPoll = True
713:                        Me.NextOperation(oFraudUpdateHQ.SaturdayDuration, _
                                              oFraudUpdateHQ.SaturdayDurationType, _
                                              oFraudUpdateHQ.SaturdayPollDuration, oFraudUpdateHQ.SaturdayNextPoll, _
                                              oFraudUpdateHQ.SaturdayPollStop)

                        End If
714:                    If oFraudUpdateHQ.SaturdayPollStop >= nextPollTime Then
715:                        shouldPoll = True
                        End If

716:                ElseIf dayOfWeek = 7 And oFraudUpdateHQ.SundayEnabled Then

717:                    If nextPollTime >= oFraudUpdateHQ.SundayNextPoll Then
718:                        shouldPoll = True
719:                        Me.NextOperation(oFraudUpdateHQ.SundayDuration, _
                                              oFraudUpdateHQ.SundayDurationType, _
                                              oFraudUpdateHQ.SundayPollDuration, oFraudUpdateHQ.SundayNextPoll, _
                                              oFraudUpdateHQ.SundayPollStop)

                        End If
720:                    If oFraudUpdateHQ.SundayPollStop >= nextPollTime Then
721:                        shouldPoll = True
                        End If
                    End If ' dayOfWeek
722:                If shouldPoll Then
723:                    Service1.serverName = oFraudUpdateHQ.ServerName
724:                    Service1.appName = oFraudUpdateHQ.ApplicationName
725:                    Me.oTransactionService = New TransactionsWebService.TransactionsService
726:                    SetProxy()
727:                    Me.RegisterComputerWithHQ()
728:                    Me.UpdateFraud()
729:                    Me.FraudAlertUpdate()
730:                    oFraudUpdateHQ.Save()
                    End If

                End If 'FraudUpdate

            End If
731:        If Not Me.oDataMoverHQ.EOF Then
732:            If oDataMoverHQ.SettingsFromHQ And Date.Now > oDataMoverHQ.SettingsFromHQTime Then
733:                While Date.Now > oDataMoverHQ.SettingsFromHQTime
734:                    oDataMoverHQ.SettingsFromHQTime = oDataMoverHQ.SettingsFromHQTime.AddDays(1)
                    End While
735:                oDataMoverHQ.Save()
736:                Me.GetSettingsFromHQ()
                End If
737:            If oDataMoverHQ.StatusToHQ And Date.Now > oDataMoverHQ.StatusToHQTime Then
738:                While Date.Now > oDataMoverHQ.StatusToHQTime
739:                    oDataMoverHQ.StatusToHQTime = oDataMoverHQ.StatusToHQTime.AddDays(1)
                    End While
740:                oDataMoverHQ.Save()
741:                Me.RegisterComputerWithHQ()
742:                Me.RegisterPOCsWithHQ()
                End If
            End If
743:    Catch ex As Exception
744:        Me.LogError("MoveData", ex.Message, Date.Now)
        Finally
745:        Me.oTranscopes = Nothing
756:        Me.oTranscopeInstruments = Nothing
757:        Me.oFraudUpdateHQ = Nothing
758:        Me.oDataMoverHQ = Nothing
            Try
759:            TransactionMgr.ThreadTransactionMgrReset()
760:        Catch ex As Exception

            End Try
761:        transmgr = Nothing
762:        If Not (Me.oTransactionService Is Nothing) Then oTransactionService.Dispose()
763:        oTransactionService = Nothing
        End Try
    End Sub

    Private Sub UpdateFraud()
764:    Dim oFraud As FraudulentActivities
        Try
765:        Dim startID As Long = 0
            'oFraud = New FraudulentActivities(ConnectionClass.strMgrCnn)
            'oFraud.LoadAll()
            'While Not oFraud.EOF

            'End While
            While True
766:            Dim photograph() As Byte
767:            Dim instrument() As Byte
768:            Dim Fingers()() As Byte
769:            Dim fraudDate As Date
770:            Dim reply() As String

                'oFraud.Where.ID.Operator = WhereParameter.Operand.GreaterThan
                'oFraud.Where.ID.Value = 0
                'oFraud.Query.AddOrderBy(FraudulentActivities.ColumnNames.ID, WhereParameter.Dir.DESC)
                'oFraud.Query.Top = 1
                'oFraud.Query.Load()
                'If Not oFraud.EOF Then
                '    startID = oFraud.ID
                'End If
                If Not oTransactionService.NextFraudCase(startID, reply, photograph, fraudDate) Then
                    Exit While
                Else
774:                oFraud = New FraudulentActivities(ConnectionClass.strMgrCnn)
775:                oFraud.Where.ID.Operator = WhereParameter.Operand.Equal
776:                oFraud.Where.ID.Value = reply(6)
777:                oFraud.Query.Load()
778:                If oFraud.EOF Then
779:                    oFraud = New FraudulentActivities(ConnectionClass.strMgrCnn)
780:                    oFraud.AddNew()
                    End If
781:                oFraud.NewxNo = reply(0)
782:                oFraud.FraudLocation = reply(1)
783:                oFraud.Status = reply(3)
784:                oFraud.s_PersonNames = reply(4)
785:                oFraud.Cleared = IIf(Boolean.Parse(reply(5)), 1, 0)
786:                oFraud.Photo = photograph
787:                oFraud.FraudDate = fraudDate
788:                oFraud.G = 1
789:                oFraud.FingerPrintTemplate = photograph
790:                oFraud.PersonID = 0
791:                oFraud.ID = reply(6)
                    'oFraud.Save()
792:                startID = Long.Parse(reply(6))
                    'If Not oTransactionService.nextFraudInstrument(startID, instrument, "") Then
                    '    Continue While
                    'End If
                    'oFraud.Instrument = instrument
793:                If Not oTransactionService.nextFraudFingers(startID, Fingers, "") Then
                        Continue While
                    End If
794:                Try : oFraud.LeftThumb = Fingers(0) : Catch ex As Exception : End Try
795:                Try : oFraud.LeftIndex = Fingers(1) : Catch ex As Exception : End Try
796:                Try : oFraud.LeftMiddle = Fingers(2) : Catch ex As Exception : End Try
797:                Try : oFraud.LeftRing = Fingers(3) : Catch ex As Exception : End Try
798:                Try : oFraud.LeftLittle = Fingers(4) : Catch ex As Exception : End Try
799:                Try : oFraud.RightThumb = Fingers(5) : Catch ex As Exception : End Try
800:                Try : oFraud.RightIndex = Fingers(6) : Catch ex As Exception : End Try
801:                Try : oFraud.RightMiddle = Fingers(7) : Catch ex As Exception : End Try
802:                Try : oFraud.RightRing = Fingers(8) : Catch ex As Exception : End Try
803:                Try : oFraud.RightLittle = Fingers(9) : Catch ex As Exception : End Try
804:            End If
805:            oFraud.FraudDetails = reply(2)
806:            oFraud.Status = reply(3)
                'oFraud.Cleared = Boolean.Parse(reply(5))
807:            oFraud.Save()

            End While

808:        oFraud = New FraudulentActivities(ConnectionClass.strMgrCnn)
809:        oFraud.LoadAll()
810:        If oFraud.RowCount > 0 Then
811:            Dim ids(oFraud.RowCount - 1) As Long
812:            Dim i As Integer = 0
813:            For i = 0 To ids.Length - 1
814:                ids(i) = oFraud.ID
815:                oFraud.MoveNext()
                Next
816:            Dim cleared() As Boolean = oTransactionService.ClearedStatusFrauds(ids)
817:            oFraud.Rewind()
818:            For i = 0 To ids.Length - 1
819:                If cleared(i) Then
820:                    oFraud = New FraudulentActivities(ConnectionClass.strMgrCnn)
821:                    If oFraud.LoadByPrimaryKey(ids(i)) Then
822:                        oFraud.MarkAsDeleted()
823:                        oFraud.Save()
                        End If
                    End If
                Next
            End If


824:    Catch ex As Exception
825:        Me.LogError("UpdateFraud", ex.Message, Date.Now)
        End Try
    End Sub

    Private Sub FraudAlertUpdate()
826:    Dim oFraudAlert As fraudalerts
827:    Dim oReg As RegistrationCentres
        Try
828:        Dim lastId As Long = 0
829:        oReg = New RegistrationCentres(ConnectionClass.strMgrCnn)
830:        oReg.LoadAll()
            While True
831:            oFraudAlert = New fraudalerts(ConnectionClass.strMgrCnn)
832:            oFraudAlert.Where.PolledToHQ.Operator = WhereParameter.Operand.NotEqual
823:            oFraudAlert.Where.PolledToHQ.Value = 1
834:            oFraudAlert.Where.ID.Operator = WhereParameter.Operand.GreaterThan
835:            oFraudAlert.Where.ID.Value = lastId
836:            oFraudAlert.Query.AddOrderBy(fraudalerts.ColumnNames.ID, WhereParameter.Dir.ASC)
837:            oFraudAlert.Query.Top = 1
838:            oFraudAlert.Query.Load()
839:            If oFraudAlert.EOF Then
                    Exit While
                Else
840:                lastId = oFraudAlert.ID
841:                If Me.oTransactionService.InsertFraudAlert(oFraudAlert.s_NewXNo, oFraudAlert.s_UserID, _
                                                                 oFraudAlert.s_Action, oFraudAlert.AlertDate, _
                                                                 oReg.RegistrationCentreID, "hdkdkidiu876wwwwggg6524242", True, _
                                                                 computerName) = "Successfull" Then
842:                    oFraudAlert.PolledToHQ = 1
843:                    oFraudAlert.Save()
                    End If
                End If
            End While

        Catch ex As Exception
844:        Me.LogError("FraudAlertUpdate", ex.Message, Date.Now)
        End Try
    End Sub

    Private Sub PollHQTransactionsWithFingerPrints()
845:    Dim oReg As New RegistrationCentres(ConnectionClass.strMgrCnn)
846:    Dim totalRecords As Integer = 0
847:    Dim totalRecordsPolled As Integer = 0
        Dim totalBytesPolled As Long = 0
        Try
848:        Dim parameters(14) As String
849:        Dim dateParameters(1) As Date
850:        Dim photograph() As Byte
851:        Dim Fingers(9)() As Byte
852:        oReg.LoadAll()
853:        Dim lastDate As Date = Date.Now.AddYears(-100)

854:        oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
855:        oTranscopes.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
856:        oTranscopes.Where.PolledToHQ.Value = 0
            'oTranscopes.Query.Top = 1
857:        oTranscopes.Query.AddResultColumn(Transcopes.ColumnNames.XNo)
858:        oTranscopes.Query.Load()
859:        If Not oTranscopes.EOF Then totalRecords = oTranscopes.RowCount
860:        oTranscopes = Nothing
            While True
861:            oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
862:            oTranscopes.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
863:            oTranscopes.Where.PolledToHQ.Value = 0
864:            oTranscopes.Where.CaptureDateTime.Operator = WhereParameter.Operand.GreaterThan
865:            oTranscopes.Where.CaptureDateTime.Value = lastDate
866:            oTranscopes.Query.Top = 1
867:            oTranscopes.Query.AddOrderBy(Transcopes.ColumnNames.CaptureDateTime, WhereParameter.Dir.ASC)
868:            oTranscopes.Query.Load()
869:            If oTranscopes.EOF Then
                    Exit While
                Else
870:                lastDate = oTranscopes.CaptureDateTime
                    Try
871:                    parameters(0) = oReg.s_RegistrationCentreID
872:                    parameters(1) = oReg.s_RegistrationCentreName
873:                    parameters(2) = oTranscopes.ERPNo
874:                    parameters(3) = oTranscopes.G
875:                    parameters(4) = IIf(oTranscopes.Gendre = 1, True, False)
876:                    parameters(5) = oTranscopes.NodeID
877:                    parameters(6) = oTranscopes.Payee
878:                    parameters(7) = oTranscopes.Remark
879:                    parameters(8) = oTranscopes.TransactionType
880:                    parameters(9) = oTranscopes.UserName
881:                    parameters(10) = oTranscopes.XNo
882:                    parameters(11) = oTranscopes.Gendre
883:                    parameters(12) = oTranscopes.PersonID
884:                    parameters(13) = oTranscopes.s_LastName
885:                    parameters(14) = oTranscopes.s_OtherNames
886:                    Dim fingerBytes As Long = 0
887:                    Try : Fingers(0) = oTranscopes.LeftThumb : fingerBytes += oTranscopes.LeftThumb.Length : Catch ex As Exception : End Try
888:                    Try : Fingers(1) = oTranscopes.LeftIndex : fingerBytes += oTranscopes.LeftIndex.Length : Catch ex As Exception : End Try
889:                    Try : Fingers(2) = oTranscopes.LeftMiddle : fingerBytes += oTranscopes.LeftMiddle.Length : Catch ex As Exception : End Try
890:                    Try : Fingers(3) = oTranscopes.LeftRing : fingerBytes += oTranscopes.LeftRing.Length : Catch ex As Exception : End Try
891:                    Try : Fingers(4) = oTranscopes.LeftLittle : fingerBytes += oTranscopes.LeftLittle.Length : Catch ex As Exception : End Try
892:                    Try : Fingers(5) = oTranscopes.RightThumb : fingerBytes += oTranscopes.RightThumb.Length : Catch ex As Exception : End Try
893:                    Try : Fingers(6) = oTranscopes.RightIndex : fingerBytes += oTranscopes.RightIndex.Length : Catch ex As Exception : End Try
894:                    Try : Fingers(7) = oTranscopes.RightMiddle : fingerBytes += oTranscopes.RightMiddle.Length : Catch ex As Exception : End Try
895:                    Try : Fingers(8) = oTranscopes.RightRing : fingerBytes += oTranscopes.RightRing.Length : Catch ex As Exception : End Try
896:                    Try : Fingers(9) = oTranscopes.RightLittle : fingerBytes += oTranscopes.RightLittle.Length : Catch ex As Exception : End Try
                        Try
897:                        photograph = oTranscopes.Photograph
898:                    Catch ex As Exception
899:                        photograph = Convert.FromBase64String("")
                        End Try

900:                    dateParameters(0) = oTranscopes.CaptureDateTime
901:                    dateParameters(1) = oTranscopes.XDate
902:                    'If Not oTranscopes.Gendre Then
                        '    If oTransaction.InsertTransaction(parameters, dateParameters, photograph, "") Then
                        '        oTranscopes.Gendre = True
                        '        oTranscopes.Save()
                        '    End If
                        'End If
903:                    oTranscopeInstruments = New TranscopeInstruments(ConnectionClass.strMgrCnn)
904:                    oTranscopeInstruments.Where.XNo.Operator = WhereParameter.Operand.Equal
905:                    oTranscopeInstruments.Where.XNo.Value = oTranscopes.XNo
906:                    oTranscopeInstruments.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
907:                    oTranscopeInstruments.Where.PolledToHQ.Value = 0
908:                    oTranscopeInstruments.Query.AddOrderBy(TranscopeInstruments.ColumnNames.ID, WhereParameter.Dir.ASC)
909:                    oTranscopeInstruments.Query.Load()

910:                    Dim instrParameters(5) As String
911:                    Dim instruments() As Byte
912:                    Dim instruments_Rear() As Byte
913:                    Dim count As Integer = 0
914:                    Dim answer As Boolean = False
915:                    While Not oTranscopeInstruments.EOF
916:                        instrParameters(0) = oTranscopeInstruments.s_AccName
917:                        instrParameters(1) = oTranscopeInstruments.s_AccNo
918:                        instrParameters(2) = oTranscopeInstruments.Amount
919:                        instrParameters(3) = oTranscopeInstruments.s_InstrumentNo
920:                        Try
921:                            instruments = oTranscopeInstruments.Instrument
922:                        Catch ex As Exception
923:                            instruments = Convert.FromBase64String("")
924:                        End Try
925:
                            Try
926:                            instruments_Rear = oTranscopeInstruments.Instrument_Rear
927:                        Catch ex As Exception
928:                            instruments_Rear = Convert.FromBase64String("")
                            End Try
929:                        instrParameters(4) = oTranscopeInstruments.Remark
930:                        Try
931:                            instrParameters(5) = oTranscopeInstruments.MICRDetails
932:                        Catch ex As Exception
933:                            instrParameters(5) = ""
934:                        End Try
                            'ADDCODE
935:                        If Not oTransactionService.InsertInstrumentWithMICR(oTranscopes.XNo, instruments, instruments_Rear, _
                                                                  instrParameters, "h7b4cx5yfc4fubv8n6v5gu776", _
                                                                  branchCode, True, computerName) Then
936:                            Throw New Exception("Error Inserting Instrument")
                            Else
937:                            totalBytesPolled += instruments.Length
938:                        End If
939:                        oTranscopeInstruments.PolledToHQ = 1
940:                        oTranscopeInstruments.Save()
941:
942:                        oTranscopeInstruments.MoveNext()
                        End While
943:                    If Not oTranscopes.Processing Then
944:                        If oTransactionService.InsertTransaction(parameters, dateParameters, photograph, "hdgd7b363gb363g6dede33", _
                                                                     branchCode, True, computerName) Then
945:                            totalBytesPolled += photograph.Length
946:                            oTranscopes.Processing = 1
947:                            oTranscopes.Save()
                            End If
                        End If
948:                    If oTranscopes.Processing Then
949:                        If oTransactionService.InsertFingers(oTranscopes.XNo, Fingers, "87678hhfy655gfgf76ttbghj", _
                                                           branchCode, True, computerName) Then
950:                            oTranscopes.PolledToHQ = 1
951:                            oTranscopes.Save()
952:                            totalRecordsPolled += 1
953:                            totalBytesPolled += fingerBytes

                            End If
                        End If
                        'If oTransaction.InsertFingers(oTranscopes.XNo, Fingers, "") Then
                        '    oTranscopes.Polled = True
                        '    oTranscopes.Save()
                        'End If
954:                Catch ex As Exception
955:                    Me.LogError("PollHQTransLoop", ex.Message, Date.Now)
                    Finally

                    End Try

                End If
            End While

            'Log HQ
            Try
966:            Me.oTransactionService.InsertLog(branchCode, computerName, _
                                                   totalRecords, totalRecordsPolled, _
                                                   totalBytesPolled, Date.Now, "8767dhdjd766dhbdgv13265xx", True, _
                                                   computerName)
967:        Catch ex As Exception

            End Try
            'End Log HQ

            'purging
968:        oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
969:        oTranscopes.Where.Polled.Operator = WhereParameter.Operand.Equal
970:        oTranscopes.Where.Polled.Value = True
971:        oTranscopes.Query.AddOrderBy(Transcopes.ColumnNames.CaptureDateTime, WhereParameter.Dir.ASC)
972:        oTranscopes.Query.AddResultColumn(Transcopes.ColumnNames.XNo)
973:        oTranscopes.Query.Load()
974:        Dim recordsToDelete As Integer = 0
975:        If oTranscopes.RowCount > PurgingVolume Then
976:            recordsToDelete = oTranscopes.RowCount - PurgingVolume
977:            While recordsToDelete > 0
978:                oTranscopeInstruments = New TranscopeInstruments(ConnectionClass.strMgrCnn)
979:                oTranscopeInstruments.Where.XNo.Operator = WhereParameter.Operand.Equal
980:                oTranscopeInstruments.Where.XNo.Value = oTranscopes.XNo
981:                oTranscopeInstruments.Where.Polled.Operator = WhereParameter.Operand.Equal
982:                oTranscopeInstruments.Where.Polled.Value = False
983:                oTranscopeInstruments.Query.AddOrderBy(TranscopeInstruments.ColumnNames.ID, WhereParameter.Dir.ASC)
984:                oTranscopeInstruments.Query.Load()
985:                While Not oTranscopeInstruments.EOF
986:                    oTranscopeInstruments.MarkAsDeleted()
987:                    oTranscopeInstruments.Save()
988:                    oTranscopeInstruments.MoveNext()
                    End While
989:                oTranscopes.MarkAsDeleted()
990:                oTranscopes.Save()
991:                oTranscopes.MoveNext()
992:                recordsToDelete -= 1
993:            End While
            End If
994:    Catch ex As Exception

995:        Me.LogError("PollTransactions", ex.Message, Date.Now)
        Finally
996:        oTranscopes = Nothing
997:        oTranscopeInstruments = Nothing
            'If Not oTransaction Is Nothing Then
            '    oTransaction.Dispose()
            '    oTransaction = Nothing
            'End If

        End Try
    End Sub

    Private Sub PollHQTransactions()
998:    Dim oReg As New RegistrationCentres(ConnectionClass.strMgrCnn)
999:    Dim totalRecords As Integer = 0
1000:   Dim totalRecordsPolled As Integer = 0
1001:   Dim totalBytesPolled As Long = 0
        oTransactionService = New TransactionsWebService.TransactionsService

        Try
1002:       Dim photograph As String
1003:       oReg.LoadAll()
1004:       Dim lastDate As Date = Date.Now.AddYears(-100)

1005:       oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
1006:       oTranscopes.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
1007:       oTranscopes.Where.PolledToHQ.Value = 0
            'oTranscopes.Query.Top = 1
1008:       oTranscopes.Query.AddResultColumn(Transcopes.ColumnNames.XNo)
1009:       oTranscopes.Query.Load()
1010:       If Not oTranscopes.EOF Then totalRecords = oTranscopes.RowCount
1011:       oTranscopes = Nothing
            While True
1012:           oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
1013:           oTranscopes.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
1014:           oTranscopes.Where.PolledToHQ.Value = 0
1015:           oTranscopes.Where.CaptureDateTime.Operator = WhereParameter.Operand.GreaterThan
1016:           oTranscopes.Where.CaptureDateTime.Value = lastDate
1017:           oTranscopes.Query.Top = 1
                '1018:           oTranscopes.Query.AddOrderBy(Transcopes.ColumnNames.CaptureDateTime, WhereParameter.Dir.ASC)
1019:           oTranscopes.Query.Load()
1020:           If oTranscopes.EOF Then
                    Exit While
                Else
1021:               lastDate = oTranscopes.CaptureDateTime
                    Try
                        'insert the transcope to HQ first
                        Try
1022:                       photograph = Convert.ToBase64String(oTranscopes.Photograph)
1023:                   Catch ex As Exception
1024:                       photograph = ""
                        End Try

                        'append random number transcopes  that has been polled
                        '						Dim rnd As New Random()						
                        '10249990:               Dim RandomNumber As Integer = rnd.Next(10000, 100000)
                        '10249991:               Dim TranscopeExists As Boolean
                        '                        Dim TranscopeExistsStr As String = Nothing
                        '                        Try
                        '                            TranscopeExistsStr = oTransactionService.FetchTranscope(oTranscopes.XNo)
                        '                        Catch ex As Exception
                        '                            Me.LogError("PollHQTransLoop TranscopeFetch", ex.Message + " Inner Exception: " + ex.InnerException.Message, Date.Now)
                        '                        End Try



                        '                        If Not (TranscopeExistsStr = "False" Or TranscopeExistsStr = "True") Then
                        '                            Throw New Exception("Error Checking for duplicate transcope")
                        '                        End If
                        '                        TranscopeExists = CBool(TranscopeExistsStr)
                        '10249992:               Dim ComputedXNO As String = IIf(TranscopeExists, "DPLK" + RandomNumber.ToString() + oTranscopes.XNo, oTranscopes.XNo)
                        'Me.LogError("PollHQTransLoop2", " Processing " + oTranscopes.XNo, Date.Now)
                        Me.LogError("PollTransaction InsertTranscope", "Processing ...pollling " + oTranscopes.XNo, Date.Now)
                        Dim gender As Integer = Convert.ToInt32(oTranscopes.Gendre)
1025:                   If oTransactionService.InsertTranscope(oTranscopes.XNo, oTranscopes.NodeID, oReg.RegistrationCentreID, oReg.RegistrationCentreName, _
                                                            oTranscopes.UserName, oTranscopes.ERPNo, oTranscopes.Remark, oTranscopes.Payee, _
                                                            gender, oTranscopes.XDate, oTranscopes.LastName, oTranscopes.OtherNames, _
                                                            photograph, oTranscopes.TransactionType, True, computerName, oReg.s_RegistrationCentreID, "h7b4cx5yfc4fubv8n6v5gu776") Then

1026:                       oTranscopeInstruments = New TranscopeInstruments(ConnectionClass.strMgrCnn)
1027:                       oTranscopeInstruments.Where.XNo.Operator = WhereParameter.Operand.Equal
1028:                       oTranscopeInstruments.Where.XNo.Value = oTranscopes.XNo
1029:                       oTranscopeInstruments.Where.PolledToHQ.Operator = WhereParameter.Operand.Equal
1030:                       oTranscopeInstruments.Where.PolledToHQ.Value = 0
1031:                       oTranscopeInstruments.Query.AddOrderBy(TranscopeInstruments.ColumnNames.ID, WhereParameter.Dir.ASC)
1032:                       oTranscopeInstruments.Query.Load()

1033:                       Dim instrParameters(5) As String
                            Dim instruments As String
1034:                       Dim instruments_Rear As String
1035:                       Dim MICRDetail As String
1036:                       Dim counter As Integer = 0
1037:                       While Not oTranscopeInstruments.EOF
                                Try
1038:                               instruments = Convert.ToBase64String(oTranscopeInstruments.Instrument)
1039:                           Catch ex As Exception
1040:                               instruments = ""
                                End Try

                                Try
1041:                               instruments_Rear = Convert.ToBase64String(oTranscopeInstruments.Instrument_Rear)
1042:                           Catch ex As Exception
1043:                               instruments_Rear = ""
                                End Try
                                Try
1044:                               MICRDetail = oTranscopeInstruments.MICRDetails
1045:                           Catch ex As Exception
1046:                               MICRDetail = ""
                                End Try

1047:                           If oTransactionService.InsertTranscopeInstrument(oTranscopes.XNo, oTranscopeInstruments.AccNo, oTranscopeInstruments.AccName, _
                                                                                oTranscopeInstruments.Amount, oTranscopeInstruments.Remark, instruments, _
                                                                                instruments_Rear, MICRDetail, (Convert.ToInt32(oTranscopeInstruments.InstrumentNo)) - 1, _
                                                                                True, computerName, oReg.s_RegistrationCentreID, "h7b4cx5yfc4fubv8n6v5gu776") Then

1048:                               totalBytesPolled += Convert.FromBase64String(instruments).Length
1049:                               totalBytesPolled += Convert.FromBase64String(instruments_Rear).Length

1050:                               oTranscopeInstruments.PolledToHQ = 1
                                    oTranscopes.Polled = 1
1051:                               oTranscopeInstruments.Save()
1052:                               counter += 1
                                Else
1053:                               Throw New Exception("Error Inserting Transcope Instrument to Transactions")
                                End If



1054:                           oTranscopeInstruments.MoveNext()
                            End While


1055:                       If counter = 0 Then
1056:                           If Not oTransactionService.InsertTranscopeInstrument(oTranscopes.XNo, "", "", _
                                                                                  0, oTranscopes.Remark, "", _
                                                                                  "", "", 0, _
                                                                                  True, computerName, oReg.s_RegistrationCentreID, "h7b4cx5yfc4fubv8n6v5gu776") Then
1057:                               'Throw New Exception("Error Inserting Only Transcope to Transactions")
                                End If
                            End If


1058:                       totalBytesPolled += Convert.FromBase64String(photograph).Length
1059:                       oTranscopes.Processing = 1
1060:                       oTranscopes.PolledToHQ = 1
1061:                       oTranscopes.Polled = 1
1062:                       oTranscopes.Save()
1063:                       totalRecordsPolled += 1

1064:                       oTransactionService.DeleteTranscope(oTranscopes.XNo)
                        Else
1065:                       Throw New Exception("Error Inserting Transcope Header")
                        End If

1066:               Catch ex As Exception
1067:                   Me.LogError("PollHQTransLoop4", ex.Message + " " + ex.InnerException.Message, Date.Now)
                    Finally

                    End Try

                End If
            End While

            'Log HQ
            Try
1068:           Me.oTransactionService.InsertLog(branchCode, computerName, _
                                                   totalRecords, totalRecordsPolled, _
                                                   totalBytesPolled, Date.Now, "8767dhdjd766dhbdgv13265xx", True, _
                                                   computerName)
1069:       Catch ex As Exception

            End Try
            'End Log HQ

            'purging
1070:       oTranscopes = New Transcopes(ConnectionClass.strMgrCnn)
1071:       oTranscopes.Where.Polled.Operator = WhereParameter.Operand.Equal
1072:       oTranscopes.Where.Polled.Value = True
1073:       oTranscopes.Query.AddOrderBy(Transcopes.ColumnNames.CaptureDateTime, WhereParameter.Dir.ASC)
1074:       'oTranscopes.Query.AddResultColumn(transcopes.ColumnNames.XNo)
1075:       oTranscopes.Query.Load()
1076:       Dim recordsToDelete As Integer = 0
1077:       If oTranscopes.RowCount > PurgingVolume Then
1078:           recordsToDelete = oTranscopes.RowCount - PurgingVolume
1079:           While recordsToDelete > 0
1080:               oTranscopeInstruments = New TranscopeInstruments(ConnectionClass.strMgrCnn)
1081:               oTranscopeInstruments.Where.XNo.Operator = WhereParameter.Operand.Equal
1082:               oTranscopeInstruments.Where.XNo.Value = oTranscopes.XNo
1083:               oTranscopeInstruments.Where.Polled.Operator = WhereParameter.Operand.Equal
1084:               oTranscopeInstruments.Where.Polled.Value = False
1085:               oTranscopeInstruments.Query.AddOrderBy(TranscopeInstruments.ColumnNames.ID, WhereParameter.Dir.ASC)
1086:               oTranscopeInstruments.Query.Load()
1087:               While Not oTranscopeInstruments.EOF
1088:                   oTranscopeInstruments.MarkAsDeleted()
1089:                   oTranscopeInstruments.Save()
1090:                   oTranscopeInstruments.MoveNext()
1091:               End While
1092:               oTranscopes.MarkAsDeleted()
1093:               oTranscopes.Save()
1094:               oTranscopes.MoveNext()
1095:               recordsToDelete -= 1
                End While
            End If

        Catch ex As Exception
1096:       Me.LogError("PollTransactions", ex.Message + IIf(IsNothing(oTranscopes), " ", oTranscopes.XNo), Date.Now)
        Finally
1097:       oTranscopes = Nothing
1098:       oTranscopeInstruments = Nothing
            'If Not oTransaction Is Nothing Then
            '    oTransaction.Dispose()
            '    oTransaction = Nothing
            'End If

        End Try
    End Sub
    Private Sub LogError(ByVal moduleName As String, ByVal errorMessage As String, _
                         ByVal errorTime As Date)
        Dim oStreamWriter As StreamWriter
        Dim fileName As String
        Try
            fileName = My.Application.Info.DirectoryPath & "\datamovererrorfile.txt"
            'oFileStream = New FileStream(fileName, FileMode.OpenOrCreate, FileAccess.ReadWrite)
            oStreamWriter = New StreamWriter(fileName, True)
            oStreamWriter.WriteLine(moduleName & vbTab & errorTime.ToShortDateString & " " & errorTime.ToShortTimeString & vbTab & errorMessage & _
            " Line Number : " & Erl() & " Description : " & Err.Description())

        Catch ex As Exception
        Finally
            If Not (oStreamWriter Is Nothing) Then
                oStreamWriter.Close()
                oStreamWriter = Nothing
            End If
        End Try
    End Sub

    '    Private Sub LogError(ByVal moduleName As String, ByVal exceptionObject As Exception, ByVal errorTime As Date)
    '1099:   Dim oStreamWriter As StreamWriter
    '1100:   Dim fileName As String
    '        Try
    '1101:       Dim st As New StackTrace(exceptionObject, True)
    '1102:       Dim sf As StackFrame = st.GetFrame(0)
    '1103:       Dim message As String
    '1104:       message = "File Name: " & sf.GetFileName() & vbTab & "Method: " & sf.GetMethod().ToString() & vbTab & exceptionObject.Message.ToString & "Line Number: " & sf.GetFileLineNumber()
    '            Me.LogError("PollHQTransLoop", message, Date.Now)
    '1105:       fileName = My.Application.Info.DirectoryPath & "\datamovererrorfile.txt"
    '            oFileStream = New FileStream(fileName, FileMode.OpenOrCreate, FileAccess.ReadWrite)
    '1106:       oStreamWriter = New StreamWriter(fileName, True)
    '1107:       oStreamWriter.WriteLine(moduleName & vbTab & errorTime.ToShortDateString & " " & errorTime.ToShortTimeString & vbTab & message)

    '1108:   Catch ex As Exception
    '        Finally
    '1109:       If Not (oStreamWriter Is Nothing) Then
    '1110:           oStreamWriter.Close()
    '1111:           oStreamWriter = Nothing
    '            End If
    '        End Try
    '    End Sub
    

End Class
