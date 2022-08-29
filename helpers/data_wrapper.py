from pathlib import Path
from typing import Union

from data_helper import DataHelper
import pandas as pd
import geopandas as gpd


class DataWrapper:
    survey: pd.DataFrame
    tracks: gpd.GeoDataFrame

    def __init__(self, version: str, data_folder_path: Union[str, Path],
                 **kwargs):
        self.dh = DataHelper(data_folder_path, **kwargs)
        self.version = version

    def load_version(self,
                     single_tracks_filename: str = None,
                     download_missing_tracks: bool = True):
        self.survey = self.dh.load_survey(self.version)
        if single_tracks_filename:
            self.tracks = self.dh.load_tracks_unifile(self.version, single_tracks_filename)
        else:
            if download_missing_tracks:
                self.tracks = self.dh.load_or_download_tracks(
                    self.version, list(self.survey.bc_token)
                )
            else:
                self.tracks = self.dh.load_tracks(
                    self.version, restrict_to=list(self.survey.bc_token)
                )

    def save_tracks_as_single_file(self, filename: str):
        self.dh.save_tracks_unifile(self.tracks, self.version, filename)

    def load_sample(self, frac: float, download_missing_tracks: bool = True):
        self.survey = self.dh.load_survey(self.version)
        tokens = list(self.survey.bc_token.sample(frac=frac, random_state=17))
        if download_missing_tracks:
            self.tracks = self.dh.load_or_download_tracks(
                self.version, list(tokens)
            )
        else:
            self.tracks = self.dh.load_tracks(
                self.version, restrict_to=list(tokens)
            )
