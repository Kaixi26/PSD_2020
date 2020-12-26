package Models.CommunicationProtocols.Requests;

import Models.CommunicationProtocols.CommunicationType;

public enum RequestTypes implements CommunicationType {
    AnnounceDistrictServer, NotifyLocation, NotifyInfection, ProbeLocation
}
